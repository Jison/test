package cn.ciyo.rpc.client

import java.net.InetSocketAddress

import akka.actor.Status.Failure
import akka.actor.{ActorLogging, ActorRef, Cancellable, FSM, Props}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import cn.ciyo.rpc.FramedBuffer

import scala.concurrent.duration._

object Connector {
  def props(remote: InetSocketAddress, manager: ActorRef) = Props(classOf[Connector], remote, manager)

  private case class ReceiveTimeout(seq: Int)
  private[client] class WarppedRequest(request: Request, val requester: ActorRef) extends Request {
    def destination = request.destination
    def deadline = request.deadline
    def body = request.body
  }

  private[client] case object Connect
  private[client] case object Stop
  private[client] case class DisconnectWhileIdle(remote: InetSocketAddress)

  private[client] sealed trait State
  private[client] object Unconnect extends State
  private[client] object Connecting extends State
  private[client] object Connected extends State
  private[client] object Disconnecting extends State
  private[client] object Disconnected extends State

  private[client] sealed case class Data(request: Option[WarppedRequest], connection: Option[ActorRef])
}

class Connector(val remote: InetSocketAddress, manager: ActorRef) extends FSM[Connector.State, Connector.Data] with ActorLogging {
  import Connector._
  import context.dispatcher

  private val receiveBuffer: FramedBuffer = new FramedBuffer()
  private var timeoutCanceller: Cancellable = _
  private var receivedStop: Boolean = false
  private var seqId: Int = 0

  def sendRequest(connection: ActorRef, request: Request) = {
    assert(connection != null)
    val frameSizeBuf = new Array[Byte](4)
    FramedBuffer.encodeFrameSize(request.body.length, frameSizeBuf)
    connection ! Tcp.Write(ByteString(frameSizeBuf) ++ request.body, Tcp.NoAck)
    receiveBuffer.clear()
    timeoutCanceller = context.system.scheduler.scheduleOnce(request.deadline.timeLeft.max(5.seconds)) {
      self ! ReceiveTimeout(seqId)
    }
  }

  startWith(Unconnect, Data(None, None))

  when(Unconnect) {
    case Event(Connect, _) =>
      import context.system
      IO(Tcp) ! Tcp.Connect(remote)
      goto(Connecting)

    case Event(r: Request, Data(None, _)) =>
      import context.system
      IO(Tcp) ! Tcp.Connect(remote)
      seqId += 1
      goto(Connecting) using Data(Some(new WarppedRequest(r, sender())), None)

    case Event(Stop, _) =>
      receivedStop = true
      goto(Disconnected)
  }

  when(Connecting) {
    case Event(Tcp.CommandFailed(_: Tcp.Connect), Data(Some(r), _)) =>
      r.requester ! Failure(new DisconnectedException(remote))
      goto(Disconnected) using Data(None, None)

    case Event(c: Tcp.Connected, Data(requestOpt, _)) =>
      val conn = sender()
      conn ! Tcp.Register(self)
      if (requestOpt.isEmpty && receivedStop) {
        conn ! Tcp.ConfirmedClose
        goto(Disconnecting)
      } else {
        requestOpt.foreach(sendRequest(conn, _))
        goto(Connected) using Data(requestOpt, Some(conn))
      }

    case Event(r: Request, d @ Data(None, _)) =>
      seqId += 1
      stay using d.copy(request = Some(new WarppedRequest(r, sender())))

    case Event(Stop, _) =>
      receivedStop = true
      stay
  }

  when(Connected) {
    case Event(r: Request, d @ Data(None, Some(conn))) =>
      seqId += 1
      sendRequest(conn, r)
      stay using d.copy(request = Some(new WarppedRequest(r, sender())))

    case Event(Tcp.CommandFailed(w: Tcp.Write), d @ Data(Some(r), Some(conn))) =>
      //当发生写失败时,一般是因为写的参数有问题,一般不应该出现这种问题
      r.requester ! Failure(new TransportException("exception occur when writing data."))
      conn ! Tcp.ConfirmedClose
      goto(Disconnecting) using d.copy(request = None)

    case Event(Tcp.Received(data), d @ Data(Some(r), Some(conn))) =>
      receiveBuffer.write(data)
      try {
        receiveBuffer.readOne() match {
          case Some(frameBody) =>
            r.requester ! SimpleResponse(frameBody)
            if (timeoutCanceller != null) {
              timeoutCanceller.cancel()
              timeoutCanceller = null
            }
            if (receivedStop) {
              conn ! Tcp.ConfirmedClose
              goto(Disconnecting) using d.copy(request = None)
            } else {
              stay using d.copy(request = None)
            }
          case None => stay
        }
      } catch {
        case e: Exception =>
          log.error("exception occur when decoding framed data {}", e)
          r.requester ! Failure(new TransportException("exception occur when decoding framed data."))
          conn ! Tcp.Abort
          goto(Disconnecting) using d.copy(request = None)
      }

    case Event(ReceiveTimeout(timeoutSeqId), d @ Data(Some(r), Some(con))) =>
      if (seqId == timeoutSeqId) {
        r.requester ! Failure(new TimeOutException(remote))
        timeoutCanceller = null
        con ! Tcp.Abort
        goto(Disconnecting) using d.copy()
      } else {
        stay
      }

    case Event(closed: Tcp.ConnectionClosed, Data(requestOpt, _)) =>
      if (closed.isErrorClosed) {
        log.error("connection closed with error: {}", closed.getErrorCause)
      } else {
        log.debug("connection closed")
      }

      requestOpt match {
        case Some(r) => r.requester ! Failure(new DisconnectedException(remote))
        case None => manager ! DisconnectWhileIdle(remote)
      }

      if (timeoutCanceller != null) {
        timeoutCanceller.cancel()
        timeoutCanceller = null
      }
      goto(Disconnected) using Data(None, None)

    case Event(Stop, d @ Data(requestOpt, Some(conn))) =>
      receivedStop = true
      if (requestOpt.isEmpty) {
        conn ! Tcp.ConfirmedClose
        goto(Disconnecting)
      } else stay
  }

  when(Disconnecting) {
    case Event(r: Request, _) =>
      sender() ! Failure(new DisconnectedException(remote))
      stay

    case Event(closed: Tcp.ConnectionClosed, _) =>
      goto(Disconnected) using Data(None, None)

    case Event(Stop, _) =>
      receivedStop = true
      stay
  }

  when(Disconnected) {
    case Event(r: Request, _) =>
      sender() ! Failure(new DisconnectedException(remote))
      stay

    case Event(Stop, _) =>
      context stop self
      stay
  }

  whenUnhandled {
    case Event(r: Request, Data(Some(_), _)) =>
      sender() ! Failure(new BusyException(remote))
      stay
  }

  onTransition {
    case _ -> Disconnected =>
      if (receivedStop) {
        context stop self
      }
  }

}
