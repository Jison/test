package cn.ciyo.rpc.client.connection

import java.util.concurrent.atomic.AtomicInteger

import akka.stream._
import akka.stream.actor.{ActorPublisher, ActorSubscriber, _}
import akka.stream.scaladsl._
import akka.actor._
import akka.stream.impl.SeqActorName
import akka.util.ByteString
import cn.ciyo.rpc.PacketFlow.{Packet, inFlow => packetInFlow, outFlow => packetOutFlow}
import org.reactivestreams.{Processor, Subscriber, Subscription}

import scala.util.{Failure, Success}

private object PoolSlot {
  import PoolFlow.{RequestContext, ResponseContext}

	sealed trait SlotCommand
	object SlotCommand {
		final case class DispatchCommand(rc: RequestContext) extends SlotCommand
		case object ConnectEagerlyCommand extends SlotCommand
	}

  sealed trait ProcessorOut
  final case class ResponseDelivery(response: ResponseContext) extends ProcessorOut
  sealed trait SlotEvent extends ProcessorOut
  object SlotEvent {
    final case class RequestCompleted(slotIx: Int) extends SlotEvent
    final case class Disconnected(slotIx: Int, failedRequests: Int) extends SlotEvent
    final case class ConnectedEagerly(slotIx: Int) extends SlotEvent
  }

	private val slotProcessorActorName = SeqActorName("SlotProcessor")

	/*
    Stream Setup
    ============

    Request-   +-----------+              +-------------+              +-------------+     +------------+
    Context    | Slot-     |  List[       |   flatten   |  Processor-  |   doubler   |     | SlotEvent- |  Response-
    +--------->| Processor +------------->| (MapConcat) +------------->| (MapConcat) +---->| Split      +------------->
               |           |  Processor-  |             |  Out         |             |     |            |  Context
               +-----------+  Out]        +-------------+              +-------------+     +-----+------+
                                                                                                 | SlotEvent
                                                                                                 | (to Conductor
                                                                                                 |  via slotEventMerge)
                                                                                                 v
   */
	def apply(slotIx: Int, connectionFlow: Flow[ByteString, ByteString, Any], pipeliningLimit: Int) (implicit system: ActorSystem, fm: Materializer): Graph[FanOutShape2[SlotCommand, ResponseContext, SlotEvent], Any] = GraphDSL.create() { implicit b ⇒
		import GraphDSL.Implicits._

		val name = slotProcessorActorName.next()

		val slotProcessor = b.add {
			Flow.fromProcessor { () ⇒
				val actor = system.actorOf(Props(new SlotProcessor(slotIx, connectionFlow, pipeliningLimit)).withDeploy(Deploy.local), name)
				new ActorProcessor[SlotCommand, List[ProcessorOut]](actor)
			}.mapConcat(identity)
		}
		val split = b.add(Broadcast[ProcessorOut](2))

		slotProcessor ~> split.in

		new FanOutShape2(
			slotProcessor.in,
			split.out(0).collect { case ResponseDelivery(r) ⇒ r }.outlet,
			split.out(1).collect { case r: SlotEvent ⇒ r }.outlet)
	}

	import ActorPublisherMessage._
	import ActorSubscriberMessage._

	private class ActorProcessor[I, O](impl: ActorRef) extends Processor[I, O] {
		val subscriber = ActorSubscriber[I](impl)
		val publisher = ActorPublisher[O](impl)
		override def onSubscribe(s: Subscription): Unit = subscriber.onSubscribe(s)
		override def onError(t: Throwable): Unit = subscriber.onError(t)
		override def onComplete(): Unit = subscriber.onComplete()
		override def onNext(t: I): Unit = subscriber.onNext(t)
		override def subscribe(s: Subscriber[_ >: O]): Unit = publisher.subscribe(s)
	}

	private class SlotProcessor(slotIx: Int, connectionFlow: Flow[ByteString, ByteString, Any], pipeliningLimit: Int)(implicit fm: Materializer)
		extends ActorSubscriber with ActorPublisher[List[ProcessorOut]] with ActorLogging {
//		var inflightRequests = immutable.Queue.empty[RequestContext]
		var inflightRequests = scala.collection.immutable.ListMap.empty[Int, RequestContext]
		val seqCounter = new AtomicInteger(0)

		val runnableGraph = Source.actorPublisher[Packet](flowInportProps(self))
		  .via(packetOutFlow())
			.via(connectionFlow)
			.via(packetInFlow())
			.toMat(Sink.actorSubscriber[Packet](flowOutportProps(self)))(Keep.both)
			.named("SlotProcessorInternalConnectionFlow")

		override def requestStrategy = ZeroRequestStrategy

		override def receive = unconnected

		val unconnected: Receive = {
			case OnNext(SlotCommand.DispatchCommand(rc: RequestContext)) ⇒
				val (connInport, connOutport) = runnableGraph.run()
				connOutport ! Request(totalDemand)
				context.become(waitingForDemandFromConnection(connInport = connInport, connOutport = connOutport, rc))

			case OnNext(SlotCommand.ConnectEagerlyCommand) ⇒
				val (in, out) = runnableGraph.run()
				onNext(SlotEvent.ConnectedEagerly(slotIx) :: Nil)
				out ! Request(totalDemand)
				context.become(waitingEagerlyConnected(connInport = in, connOutport = out))

			case Request(_) ⇒ if (remainingRequested == 0) request(1) // ask for first request if necessary

			case OnComplete ⇒ onComplete()
			case OnError(e) ⇒ onError(e)
			case Cancel ⇒
				cancel()
				shutdown()

			case c @ FromConnection(msg) ⇒ // ignore ...
		}

		def waitingEagerlyConnected(connInport: ActorRef, connOutport: ActorRef): Receive = {
			case FromConnection(Request(n)) ⇒
				request(n)

			case OnNext(SlotCommand.DispatchCommand(rc: RequestContext)) ⇒
				handleSendRequest(connInport, rc)
				request(1)
				context.become(running(connInport, connOutport))
		}

		def waitingForDemandFromConnection(connInport: ActorRef, connOutport: ActorRef, firstRequest: RequestContext): Receive = {
			case ev @ (Request(_) | Cancel)     ⇒ connOutport ! ev
			case ev @ (OnComplete | OnError(_)) ⇒ connInport ! ev
			case OnNext(x)                      ⇒ throw new IllegalStateException("Unrequested RequestContext: " + x)

			case FromConnection(Request(n)) ⇒
				handleSendRequest(connInport, firstRequest)
				request(n - remainingRequested)
				context.become(running(connInport, connOutport))

			case FromConnection(Cancel)     ⇒ if (!isActive) { cancel(); shutdown() } // else ignore and wait for accompanying OnComplete or OnError
			case FromConnection(OnComplete) ⇒ handleDisconnect(sender(), None, Some(firstRequest))
			case FromConnection(OnError(e)) ⇒ handleDisconnect(sender(), Some(e), Some(firstRequest))
			case FromConnection(OnNext(x))  ⇒ throw new IllegalStateException("Unexpected response: " + x)
		}

		def running(connInport: ActorRef, connOutport: ActorRef): Receive = {
			case ev @ (Request(_) | Cancel)     ⇒ connOutport ! ev
			case ev @ (OnComplete | OnError(_)) ⇒ connInport ! ev
			case OnNext(SlotCommand.DispatchCommand(rc: RequestContext)) ⇒ handleSendRequest(connInport, rc)
			case FromConnection(Request(n)) ⇒ request(n)
			case FromConnection(Cancel)     ⇒ if (!isActive) { cancel(); shutdown() } // else ignore and wait for accompanying OnComplete or OnError
			case FromConnection(OnNext(receive: Packet)) ⇒ handleReceiveResponse(receive)
			case FromConnection(OnComplete) ⇒ log.debug("on complete"); handleDisconnect(sender(), None)
			case FromConnection(OnError(e)) ⇒ log.debug("on error"); handleDisconnect(sender(), Some(e))
		}

		def handleSendRequest(connInport: ActorRef, rc: RequestContext) = {
			val seq = seqCounter.getAndIncrement()
			inflightRequests = inflightRequests + ((seq, rc))
			connInport ! OnNext(Packet(Some(seq), rc.request.toByteString))
		}

		def handleReceiveResponse(response: Packet) = {
			response.seq.foreach { i =>

			}
			response.seq match {
				case Some(i) =>
					inflightRequests.get(i).foreach { rc =>
						inflightRequests = inflightRequests - i
						val delivery = ResponseDelivery(ResponseContext(rc, Success(response.data)))
						val requestCompleted = SlotEvent.RequestCompleted(slotIx)
						onNext(delivery :: requestCompleted :: Nil)
					}
				case None => throw new IllegalStateException("response with out a seq.")
			}
		}

		def handleDisconnect(connInport: ActorRef, error: Option[Throwable], firstContext: Option[RequestContext] = None): Unit = {
			log.debug("Slot {} disconnected after {}", slotIx, error getOrElse "regular connection close")

			val results: List[ProcessorOut] = {
				if (inflightRequests.isEmpty && firstContext.isDefined) {
					(error match {
						case Some(err) ⇒ ResponseDelivery(ResponseContext(firstContext.get, Failure(new UnexpectedDisconnectException("Unexpected (early) disconnect", err))))
						case _         ⇒ ResponseDelivery(ResponseContext(firstContext.get, Failure(new UnexpectedDisconnectException("Unexpected (early) disconnect"))))
					}) :: Nil
				} else {
					val reason = error.fold[Throwable](new UnexpectedDisconnectException("Unexpected disconnect"))(identity)
					inflightRequests.values.map { rc ⇒
						connInport ! ActorPublisherMessage.Cancel
						ResponseDelivery(ResponseContext(rc, Failure(reason)))
					}(collection.breakOut)
				}
			}
			inflightRequests = scala.collection.immutable.ListMap.empty
			onNext(SlotEvent.Disconnected(slotIx, results.size) :: results)
			if (canceled) onComplete()

			context.become(unconnected)
		}

		override def onComplete(): Unit = {
			super.onComplete()
			shutdown()
		}

		override def onError(cause: Throwable): Unit = {
			super.onError(cause)
			shutdown()
		}

		def shutdown(): Unit = context.stop(self)
	}

	private case class FromConnection(ev: Any) extends NoSerializationVerificationNeeded

	private class FlowInportActor(slotProcessor: ActorRef) extends ActorPublisher[Packet] with ActorLogging {
		def receive: Receive = {
			case ev: Request            ⇒ slotProcessor ! FromConnection(ev)
			case OnNext(r: Packet) ⇒ onNext(r)
			case OnComplete             ⇒ onCompleteThenStop()
			case OnError(e)             ⇒ onErrorThenStop(e)
			case Cancel ⇒
				slotProcessor ! FromConnection(Cancel)
				context.stop(self)
		}
	}
	def flowInportProps(s: ActorRef) = Props(new FlowInportActor(s)).withDeploy(Deploy.local)

	private class FlowOutportActor(slotProcessor: ActorRef) extends ActorSubscriber with ActorLogging {
		def requestStrategy = ZeroRequestStrategy
		def receive: Receive = {
			case Request(n) ⇒ request(n)
			case Cancel     ⇒ cancel()
			case ev: OnNext ⇒ slotProcessor ! FromConnection(ev)
			case ev @ (OnComplete | OnError(_)) ⇒
				slotProcessor ! FromConnection(ev)
				context.stop(self)
		}
	}
	def flowOutportProps(s: ActorRef) = Props(new FlowOutportActor(s)).withDeploy(Deploy.local)

	final class UnexpectedDisconnectException(msg: String, cause: Throwable) extends RuntimeException(msg, cause) {
		def this(msg: String) = this(msg, null)
	}
}
