package cn.ciyo.rpc.client.connection

import language.existentials
import scala.annotation.tailrec
import scala.collection.immutable
import akka.event.LoggingAdapter
import akka.stream.scaladsl._
import akka.stream._
import akka.stream.stage.GraphStage
import akka.stream.stage.GraphStageLogic
import akka.stream.stage.InHandler

private object PoolConductor {
  import PoolFlow.RequestContext
  import PoolSlot.{SlotEvent, SlotCommand}

  case class Ports(
    requestIn:   Inlet[RequestContext],
    slotEventIn: Inlet[SlotEvent],
    slotOuts:    immutable.Seq[Outlet[SlotCommand]]) extends Shape {

    override val inlets = requestIn :: slotEventIn :: Nil
    override def outlets = slotOuts

    override def deepCopy(): Shape =
      Ports(requestIn.carbonCopy(), slotEventIn.carbonCopy(), slotOuts.map(_.carbonCopy()))

    override def copyFromPorts(inlets: immutable.Seq[Inlet[_]], outlets: immutable.Seq[Outlet[_]]): Shape =
      Ports(
        inlets.head.asInstanceOf[Inlet[RequestContext]],
        inlets.last.asInstanceOf[Inlet[SlotEvent]],
        outlets.asInstanceOf[immutable.Seq[Outlet[SlotCommand]]])
  }

  final case class PoolSlotsSetting(minSlots: Int, maxSlots: Int) {
    require(minSlots <= maxSlots, "min-connections must be <= max-connections")
  }

  /*
    Stream Setup
    ============
                                                                                                  Slot-
    Request-       +-----------+    Switch-    +-------------+     +-----------+    Command
    Context        |   slot-   |    Command    |   doubler   |     |   route   +-------------->
    +------------->| Selector  +-------------->| (MapConcat) +---->|  (Flexi   +-------------->
                   |           |               |             |     |   Route)  +-------------->
                   +-----+-----+               +-------------+     +-----------+       to slots
                         ^
                         | SlotEvent
                         |
                     SlotEvent (from slotEventMerge)
  */
  def apply(slotSettings: PoolSlotsSetting, pipeliningLimit: Int, log: LoggingAdapter): Graph[Ports, Any] =
  GraphDSL.create() { implicit b ⇒
    import GraphDSL.Implicits._

    val slotSelector = b.add(new SlotSelector(slotSettings, pipeliningLimit, log))
    val route = b.add(new Route(slotSettings.maxSlots))
    slotSelector.out ~> route.in
    Ports(slotSelector.in0, slotSelector.in1, route.outArray.toList)
  }

  final case class SwitchSlotCommand(cmd: SlotCommand, slotIx: Int)

  // the SlotSelector keeps the state of all slots as instances of this ADT
  private sealed trait SlotState

  // the connection of the respective slot is not connected
  private case object Unconnected extends SlotState

  // the connection of the respective slot is connected with no requests currently in flight
  private case object Idle extends SlotState

  // the connection of the respective slot has a number of requests in flight and all of them
  // are idempotent which allows more requests to be pipelined onto the connection if required
  private final case class Loaded(openIdempotentRequests: Int) extends SlotState { require(openIdempotentRequests > 0) }

  // the connection of the respective slot has a number of requests in flight and the
  // last one of these is not idempotent which blocks the connection for more pipelined requests
  private case class Busy(openRequests: Int) extends SlotState { require(openRequests > 0) }
  private object Busy extends Busy(1)

  private class SlotSelector(slotSettings: PoolSlotsSetting, pipeliningLimit: Int, log: LoggingAdapter)
    extends GraphStage[FanInShape2[RequestContext, SlotEvent, SwitchSlotCommand]] {

    private val ctxIn = Inlet[RequestContext]("requestContext")
    private val slotIn = Inlet[SlotEvent]("slotEvents")
    private val out = Outlet[SwitchSlotCommand]("slotCommand")

    override def initialAttributes = Attributes.name("SlotSelector")

    override val shape = new FanInShape2(ctxIn, slotIn, out)

    override def createLogic(effectiveAttributes: Attributes) = new GraphStageLogic(shape) {
      val slotStates = Array.fill[SlotState](slotSettings.maxSlots)(Unconnected)
      var nextSlot = 0

      setHandler(ctxIn, new InHandler {
        override def onPush(): Unit = {
          val ctx = grab(ctxIn)
          val slot = nextSlot
          slotStates(slot) = slotStateAfterDispatch(slotStates(slot), idempotent = true)
          nextSlot = bestSlot()
          emit(out, SwitchSlotCommand(SlotCommand.DispatchCommand(ctx), slot), tryPullCtx)
        }
      })

      setHandler(slotIn, new InHandler {
        override def onPush(): Unit = {
          grab(slotIn) match {
            case SlotEvent.RequestCompleted(slotIx) ⇒
              slotStates(slotIx) = slotStateAfterRequestCompleted(slotStates(slotIx))
            case SlotEvent.Disconnected(slotIx, failed) ⇒
              slotStates(slotIx) = slotStateAfterDisconnect(slotStates(slotIx), failed)
              reconnectIfNeeded()
            case SlotEvent.ConnectedEagerly(slotIx) ⇒
            // do nothing ...
          }
          pull(slotIn)
          val wasBlocked = nextSlot == -1
          nextSlot = bestSlot()
          val nowUnblocked = nextSlot != -1
          if (wasBlocked && nowUnblocked) pull(ctxIn) // get next request context
        }
      })

      setHandler(out, eagerTerminateOutput)

      val tryPullCtx = () ⇒ if (nextSlot != -1 && !hasBeenPulled(ctxIn)) pull(ctxIn)

      override def preStart(): Unit = {
        pull(ctxIn)
        pull(slotIn)
        (0 until slotSettings.minSlots).foreach { connect }
      }

      def connect(slotIx: Int): Unit = {
        emit(out, SwitchSlotCommand(SlotCommand.ConnectEagerlyCommand, slotIx))
        slotStates(slotIx) = Idle
      }

      private def reconnectIfNeeded(): Unit =
        if (slotStates.count(_ != Unconnected) < slotSettings.minSlots) {
          connect(slotStates.indexWhere(_ == Unconnected))
        }

      def slotStateAfterDispatch(slotState: SlotState, idempotent: Boolean): SlotState =
        slotState match {
          case Unconnected | Idle ⇒ if (idempotent) Loaded(1) else Busy(1)
          case Loaded(n)          ⇒ if (idempotent) Loaded(n + 1) else Busy(n + 1)
          case Busy(_)            ⇒ throw new IllegalStateException("Request scheduled onto busy connection?")
        }

      def slotStateAfterRequestCompleted(slotState: SlotState): SlotState =
        slotState match {
          case Loaded(1) ⇒ Idle
          case Loaded(n) ⇒ Loaded(n - 1)
          case Busy(1)   ⇒ Idle
          case Busy(n)   ⇒ Busy(n - 1)
          case _         ⇒ throw new IllegalStateException(s"RequestCompleted on $slotState connection?")
        }

      def slotStateAfterDisconnect(slotState: SlotState, failed: Int): SlotState =
        slotState match {
          case Idle if failed == 0      ⇒ Unconnected
          case Loaded(n) if n > failed  ⇒ Loaded(n - failed)
          case Loaded(n) if n == failed ⇒ Unconnected
          case Busy(n) if n > failed    ⇒ Busy(n - failed)
          case Busy(n) if n == failed   ⇒ Unconnected
          case _                        ⇒ throw new IllegalStateException(s"Disconnect(_, $failed) on $slotState connection?")
        }

      @tailrec def bestSlot(ix: Int = 0, bestIx: Int = -1, bestState: SlotState = Busy): Int =
      if (ix < slotStates.length) {
        val pl = pipeliningLimit
        slotStates(ix) → bestState match {
          case (Idle, _)                           ⇒ ix
          case (Unconnected, Loaded(_) | Busy)     ⇒ bestSlot(ix + 1, ix, Unconnected)
          case (x @ Loaded(a), Loaded(b)) if a < b ⇒ bestSlot(ix + 1, ix, x)
          case (x @ Loaded(a), Busy) if a < pl     ⇒ bestSlot(ix + 1, ix, x)
          case _                                   ⇒ bestSlot(ix + 1, bestIx, bestState)
        }
      } else bestIx
    }
  }

  private class Route(slotCount: Int) extends GraphStage[UniformFanOutShape[SwitchSlotCommand, SlotCommand]] {

    override def initialAttributes = Attributes.name("PoolConductor.Route")

    override val shape = new UniformFanOutShape[SwitchSlotCommand, SlotCommand](slotCount)

    override def createLogic(effectiveAttributes: Attributes) = new GraphStageLogic(shape) {
      shape.outArray foreach { setHandler(_, ignoreTerminateOutput) }

      val in = shape.in
      setHandler(in, new InHandler {
        override def onPush(): Unit = {
          val switchCommand = grab(in)
          emit(shape.outArray(switchCommand.slotIx), switchCommand.cmd, pullIn)
        }
      })
      val pullIn = () ⇒ pull(in)

      override def preStart(): Unit = pullIn()
    }
  }
}
