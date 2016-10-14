package cn.ciyo.rpc.server

import akka.actor.{Actor, ActorLogging, Props}
import akka.routing.{DefaultOptimalSizeExploringResizer, SmallestMailboxPool}

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}
import cn.ciyo.rpc.ByteStringTransport
import org.apache.thrift.protocol.{TProtocolFactory, TProtocolUtil, TType}
import org.apache.thrift.TApplicationException

object Processor {
  def props(functionContainer: FunctionContainer, protocolFactory: TProtocolFactory) = {
    SmallestMailboxPool(4, resizer = Some(DefaultOptimalSizeExploringResizer(
      lowerBound = 4,
      upperBound = 200
    ))).props(Props(classOf[Processor], functionContainer, protocolFactory))
  }

  def echoProps() = {
    SmallestMailboxPool(4, resizer = Some(DefaultOptimalSizeExploringResizer(
      lowerBound = 4,
      upperBound = 200
    ))).props(Props(classOf[EchoProcessor]))
  }

  def lazyProps(delay: FiniteDuration) = {
    SmallestMailboxPool(4, resizer = Some(DefaultOptimalSizeExploringResizer(
      lowerBound = 4,
      upperBound = 200
    ))).props(Props(classOf[LazyEchoProcessor], delay))
  }

  case class Request(inputTransport: ByteStringTransport, outputTransport: ByteStringTransport)
}

class Processor(functionContainer: FunctionContainer, protocolFactory: TProtocolFactory) extends Actor with ActorLogging {
  import Processor._

  override def preStart = {
    log.debug("processor start!")
    super.preStart()
  }

  def receive = {
    case req: Request =>
      try {
        handleRequest(req)
      } catch {
        case e: Exception => log.error("Exception {}", e)
      }
  }

  def handleRequest(req: Request): Unit = {
    val in = protocolFactory.getProtocol(req.inputTransport)
    val msg = in.readMessageBegin()
    log.debug("receive request {}", msg)

//    functionContainer.getFunction(msg.name) match {
//      case None =>
//        log.debug("method no found {}", msg.name)
//        TProtocolUtil.skip(in, TType.STRUCT)
//        in.readMessageEnd()
//        val x = new TApplicationException(TApplicationException.UNKNOWN_METHOD,
//          "Invalid method name: '" + msg.name + "'")
//        respond(req.outputTransport, msg, Left(x))
//
//      case Some(func) =>
//        try {
//          val args = func.argsCodec.decode(in)
//          log.debug("decode args {}", args)
//          in.readMessageEnd()
//
//          if (func.isOneway || msg.`type` == TMessageType.ONEWAY) {
//            func.invoke(args)
//          } else {
//            func.invoke(args).onComplete{
//              case Failure(e) =>
//                log.debug("invoke failure {}", e)
//                val x = new TApplicationException(TApplicationException.INTERNAL_ERROR, e.getMessage)
//                respond(req.outputTransport, msg, Left(x))
//              case Success(r) =>
//                log.debug("invoke succes {}", r)
//                respond(req.outputTransport, msg, Right((r, func.resultCodec)))
//            }
//          }
//        } catch {
//          case e: TProtocolException => {
//            in.readMessageEnd()
//            val x = new TApplicationException(TApplicationException.PROTOCOL_ERROR, e.getMessage)
//            respond(req.outputTransport, msg, Left(x))
//          }
//        }
//    }
  }

//  def respond[T](transport: TTransport, msg: TMessage, either: Either[TApplicationException, (T, ThriftCodec[T])])= {
//    try {
//      transport.open()
//      val out = protocolFactory.getProtocol(transport)
//      either match {
//        case Left(e: TApplicationException) =>
//          out.writeMessageBegin(new TMessage(msg.name, TMessageType.EXCEPTION, msg.seqid))
//          e.write(out)
//        case Right((result, resultCodec)) =>
//          out.writeMessageBegin(new TMessage(msg.name, TMessageType.REPLY, msg.seqid))
//          resultCodec.encode(result, out)
//      }
//      out.writeMessageEnd()
//      out.getTransport.flush()
//    } catch {
//      case e: Exception => log.error("Exception! {}", e)
//    }
//  }

}

class EchoProcessor extends Processor(null, null) {
  import Processor._
  override def handleRequest(req: Request) = {
    val bs = req.inputTransport.getByteString
    log.info("receive {}", bs.utf8String)
    req.outputTransport.resetByteString(bs)
    req.outputTransport.flush()
  }
}

class LazyEchoProcessor(delay: FiniteDuration) extends EchoProcessor {
  import Processor._
  import context.dispatcher

  override def handleRequest(req: Request) = {
    context.system.scheduler.scheduleOnce(delay) {
      super.handleRequest(req)
    }
  }
}
