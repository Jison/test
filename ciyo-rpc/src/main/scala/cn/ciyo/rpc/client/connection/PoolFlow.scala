package cn.ciyo.rpc.client.connection

import akka.NotUsed
import akka.util.ByteString
import akka.event.LoggingAdapter
import akka.actor._
import akka.stream.{ FlowShape, Materializer }
import akka.stream.scaladsl._

import scala.concurrent.{Future, Promise}
import scala.util.Try
import cn.ciyo.rpc.model.Call

/*
    Pool Flow Stream Setup
    ======================
                                               +-------------------+
                                               |                   |
                                        +----> | Connection Slot 1 +---->
                                        |      |                   |    |
                                        |      +---+---------------+    |
                                        |          |                    |
                       +-----------+    |      +-------------------+    |      +---------------+
      RequestContext   |           +----+      |                   |    +----> |               |  ResponseContext
    +----------------> | Conductor |---------> | Connection Slot 2 +---------> | responseMerge +------------------>
                       |           +----+      |                   |    +----> |               |
                       +-----------+    |      +---------+---------+    |      +---------------+
                             ^          |          |     |              |
                             |          |      +-------------------+    |
                             |          |      |                   |    |
                RawSlotEvent |          +----> | Connection Slot 3 +---->
                             |                 |                   |
                             |                 +---------------+---+
                             |                     |     |     |
                       +-----------+  RawSlotEvent |     |     |
                       | slotEvent | <-------------+     |     |
                       |   Merge   | <-------------------+     |
                       |           | <-------------------------+
                       +-----------+
  */
object PoolFlow {
  case class RequestContext(request: Call[_], responsePromise: Promise[ByteString])
  case class ResponseContext(rc: RequestContext, response: Try[ByteString])

  def apply(
    connectionFlow: Flow[ByteString, ByteString, Future[Tcp.OutgoingConnection]],
    settings: PoolSettings,
    log: LoggingAdapter
  )(implicit system: ActorSystem, fm: Materializer): Flow[RequestContext, ResponseContext, NotUsed] = {

	  Flow.fromGraph(GraphDSL.create[FlowShape[RequestContext, ResponseContext]]() { implicit b ⇒
		  import settings._
		  import GraphDSL.Implicits._

		  val conductor = b.add(PoolConductor(PoolConductor.PoolSlotsSetting(maxSlots = maxConnections, minSlots = minConnections), pipeliningLimit, log))

		  val slots = Vector
			  .tabulate(maxConnections)(PoolSlot(_, connectionFlow, pipeliningLimit))
			  .map(b.add)

		  val responseMerge = b.add(Merge[ResponseContext](maxConnections))
		  val slotEventMerge = b.add(Merge[PoolSlot.SlotEvent](maxConnections))

		  slotEventMerge.out ~> conductor.slotEventIn

		  for ((slot, ix) ← slots.zipWithIndex) {
			  conductor.slotOuts(ix) ~> slot.in
			  slot.out0 ~> responseMerge.in(ix)
			  slot.out1 ~> slotEventMerge.in(ix)
		  }
		  FlowShape(conductor.requestIn, responseMerge.out)
	  })
  }
}
