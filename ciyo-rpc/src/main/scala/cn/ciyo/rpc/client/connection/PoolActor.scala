package cn.ciyo.rpc.client.connection

import java.net.InetSocketAddress

import akka.Done
import akka.actor._
import akka.stream.{BufferOverflowException, Materializer}
import akka.stream.actor.ActorPublisherMessage._
import akka.stream.actor.ActorSubscriberMessage._
import akka.stream.actor.{ActorPublisher, ActorSubscriber, ZeroRequestStrategy}
import akka.stream.impl.SeqActorName
import akka.stream.scaladsl._
import akka.util.ByteString
import cn.ciyo.rpc.client.connection.PoolFlow.{RequestContext, ResponseContext}
import cn.ciyo.rpc.client.connection.PoolManager.{SendRequest, ShutdownOne}

import scala.concurrent.{Future, Promise}

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.collection.mutable

object PoolActor {

	case object Shutdown extends DeadLetterSuppression

	val name = SeqActorName("PoolActor")

	def props(manager: ActorRef, address: InetSocketAddress, poolSettings: PoolSettings, connectionSettings: ConnectionSettings)(implicit fm: Materializer) = Props(new PoolActor(manager, address, poolSettings, connectionSettings)).withDeploy(Deploy.local)
}

class PoolActor(manager: ActorRef, address: InetSocketAddress, poolSettings: PoolSettings, connectionSettings: ConnectionSettings)(implicit fm: Materializer) extends ActorSubscriber with ActorPublisher[RequestContext] with ActorLogging {
	import PoolActor._

	private[this] val inputQueue = mutable.Queue.empty[RequestContext]
	private[this] var activeIdleTimeout: Option[Cancellable] = None

	log.debug("(Re-)starting host connection pool to {}", address)

	initPoolFlow()

	/** Start the pool flow with this actor acting as source as well as sink */
	private def initPoolFlow() = {
		import context.system

		val connectionFlow = ConnectionFlow(address, connectionSettings)
		val poolFlow = PoolFlow(connectionFlow, poolSettings, log).named("PoolFlow")

		Source.fromPublisher(ActorPublisher(self)).via(poolFlow).runWith(Sink.fromSubscriber(ActorSubscriber[ResponseContext](self)))
	}

	activateIdleTimeoutIfNecessary()

	def requestStrategy = ZeroRequestStrategy

	def receive = {

		/////////////// COMING UP FROM POOL (SOURCE SIDE) //////////////

		case Request(_) ⇒ dispatchRequests() // the pool is ready to take on more requests

		case Cancel     ⇒
		// somehow the pool shut down, however, we don't do anything here because we'll also see an
		// OnComplete or OnError which we use as the sole trigger for cleaning up

		/////////////// COMING DOWN FROM POOL (SINK SIDE) //////////////

		case OnNext(ResponseContext(rc, responseTry)) ⇒
			rc.responsePromise.complete(responseTry)
			activateIdleTimeoutIfNecessary()

		case OnComplete ⇒
			log.debug("Host connection pool to {} has completed orderly shutdown", address)
			self ! PoisonPill // give potentially queued requests another chance to be forwarded back to the gateway

		case OnError(e) ⇒
			log.debug("Host connection pool to {} has shut down with error {}", address, e)
			self ! PoisonPill // give potentially queued requests another chance to be forwarded back to the gateway

		/////////////// FROM CLIENT //////////////

		case x: RequestContext if isActive ⇒
			activeIdleTimeout foreach { timeout ⇒
				timeout.cancel()
				activeIdleTimeout = None
			}
			if (totalDemand == 0) {
				// if we can't dispatch right now we buffer and dispatch when demand from the pool arrives
				if (inputQueue.length >= poolSettings.maxOpenRequest) {
					x.responsePromise.failure(new BufferOverflowException(s"Exceeded configured max-open-requests value of [${poolSettings.maxOpenRequest}]"))
				} else inputQueue.enqueue(x)
			} else dispatchRequest(x)
			request(1)

		case rc @ RequestContext ⇒
			sendRequestBackToManager(rc)

		case Shutdown ⇒
			log.debug("Shutting down host connection pool to {}", address)
			onCompleteThenStop()
			while (inputQueue.nonEmpty) {
				sendRequestBackToManager(inputQueue.dequeue())
			}
	}

	@tailrec private def dispatchRequests(): Unit =
		if (totalDemand > 0 && inputQueue.nonEmpty) {
			dispatchRequest(inputQueue.dequeue())
			dispatchRequests()
		}

	def dispatchRequest(pr: RequestContext): Unit = {
		onNext(RequestContext(pr.request, pr.responsePromise))
	}

	def sendRequestBackToManager(rc: RequestContext): Unit = {
		val promise = Promise[ByteString]()
		manager ! SendRequest(address, rc)
		rc.responsePromise.completeWith(promise.future)
	}

	def shutdown(): Future[Done] = {
		val promise = Promise[Done]()
		manager ! ShutdownOne(address, promise)
		promise.future
	}

	def activateIdleTimeoutIfNecessary(): Unit =
		if (shouldStopOnIdle()) {
			import context.dispatcher
			val timeout = poolSettings.idleTimeout.asInstanceOf[FiniteDuration]
			activeIdleTimeout = Some(context.system.scheduler.scheduleOnce(timeout)(shutdown()))
		}

	private def shouldStopOnIdle(): Boolean =
		remainingRequested == 0 && poolSettings.idleTimeout.isFinite && poolSettings.minConnections == 0
}
