package cn.ciyo.rpc.server

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.io.Inet.SocketOption
import akka.pattern.{Backoff, BackoffSupervisor}
import org.slf4j.LoggerFactory

import scala.collection.immutable
import scala.concurrent.duration._

object ServiceServer {
  case class Args(address: InetSocketAddress,
                  backlog: Int = 100,
                  options: immutable.Traversable[SocketOption] = Nil,
                  pullMode: Boolean = false)

  private[rpc] val logger = LoggerFactory.getLogger(classOf[ServiceServer])

  def props(args: Args, processorProps: Props, listener: ActorRef) = Props(classOf[ServiceServer], args, processorProps, listener)
}

class ServiceServer(args: ServiceServer.Args, processorProps: Props, listener: ActorRef) extends Actor {

  val processor = context.system.actorOf(processorProps, "processor-master")
  val acceptorProps = Props(classOf[Acceptor], args, processor, listener)
  val supervisorProps = BackoffSupervisor.props(Backoff.onFailure(
    acceptorProps,
    childName = "acceptor",
    minBackoff = 5.seconds,
    maxBackoff = 30.seconds,
    randomFactor = 0.2
  ))

  val acceptorSupervisor = context.actorOf(supervisorProps, "acceptor-supervisor")

  def receive = {
    case _ =>
  }
}