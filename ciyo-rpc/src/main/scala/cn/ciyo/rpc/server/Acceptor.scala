package cn.ciyo.rpc.server

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import cn.ciyo.rpc.FramedBuffer
import cn.ciyo.rpc.InputByteStringTransport
import cn.ciyo.rpc.OutputByteStringTransport

class Acceptor(args: ServiceServer.Args, processor: ActorRef, listener: ActorRef = null) extends Actor with ActorLogging {
  import context.system

  override def preStart = {
    IO(Tcp) ! Bind(self, args.address, args.backlog, args.options, args.pullMode)
    super.preStart()
  }

  def receive = {
    case b @ Bound(localAddress) =>
      log.debug("bound {}", localAddress)
      if (listener != null) listener ! b

    case c @ CommandFailed(_: Bind) =>
      log.error("bind failed, address: {}", args.address)
      if (listener != null) listener ! c
      context stop self

    case c @ Connected(remote, local) =>
      if (listener != null) listener ! c
      log.debug("connected {}", remote)
      val handler = context.actorOf(Props(classOf[AcceptHandler], processor, listener))
      val connection = sender()
      connection ! Register(handler)
  }
}

class AcceptHandler(processor: ActorRef, listener: ActorRef = null) extends Actor with ActorLogging {
  var buffer: FramedBuffer = new FramedBuffer()

  def receive = {
    case Received(data) =>
      buffer.write(data)
      try {
        val connection = sender()
        def output = {bs: ByteString =>
          val frameSizeBuf = new Array[Byte](4)
          FramedBuffer.encodeFrameSize(-bs.length, frameSizeBuf)
          connection ! Write(ByteString(frameSizeBuf) ++ bs)
        }
        buffer.readAll().foreach {frameBody =>
          processor ! Processor.Request(InputByteStringTransport(frameBody), OutputByteStringTransport(output))
        }
      } catch {
        case e: Exception =>
          log.error("exception occur when decoding framed data {}", e)
          sender() ! Close
      }

    case CommandFailed(w: Write) =>
      //这个错误发生时应该是一次写失败了,不用断开链接,因为那些写会被完全抛弃掉.

    case closed: ConnectionClosed =>
      //PeerColosed, Closed, ConfirmedClosed, Aborted, ErrorClosed
      if (listener != null) listener ! closed
      if (closed.isErrorClosed) {
        log.error("connection closed with error: {}", closed.getErrorCause)
      } else {
        log.debug("connection closed")
      }
      context stop self
  }

}
