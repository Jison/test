package cn.ciyo.rpc.client

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Status}
import akka.pattern.ask
import akka.util.Timeout

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success}

object ConnectorMediator {

  def props(low: Int, high: Int, maxWaiting: Int) = Props(classOf[ConnectorMediator], low, high, maxWaiting)

  class WaitingListFullException(val address: InetSocketAddress) extends Exception(s"waiting list is full for address: ${address.toString}") with NoStackTrace


  private object CheckDeadline
}

class ConnectorMediator(low: Int, high: Int, maxWaiting: Int) extends Actor with ActorLogging {
  import ConnectorMediator._
  import context.dispatcher

  private class ConnectorPool(
                               val remote: InetSocketAddress,
                               val minTotal: Int,
                               val maxTotal: Int,
                               val maxWaiting: Int) {

    val idles = new mutable.ListBuffer[ActorRef]()
    var activeCount = 0
    val waitings = mutable.PriorityQueue.empty(WaitingOrder)

    object WaitingOrder extends Ordering[(Request, ActorRef)] {
      def compare(a: (Request, ActorRef), b: (Request, ActorRef)) = b._1.deadline.compare(a._1.deadline)
    }

  }

  private val poolMap = mutable.HashMap.empty[InetSocketAddress, ConnectorPool]

  private def acquire(address: InetSocketAddress): (Option[ActorRef], ConnectorPool) = {
    poolMap.get(address) match {
      case Some(pool) =>
        if (pool.idles.nonEmpty) {
          val idle = pool.idles.head
          pool.idles.remove(0)
          (Some(idle), pool)
        } else if (pool.activeCount < pool.maxTotal) {
          (Some(context.actorOf(Connector.props(address, self))), pool)
        } else {
          (None, pool)
        }
      case None =>
        val pool = new ConnectorPool(address, low, high, maxWaiting)
        poolMap.put(address, pool)
        (Some(context.actorOf(Connector.props(address, self))), pool)
    }
  }

  private def release(pool: ConnectorPool, connector: ActorRef, failure: Boolean): Unit = {
    if (failure) {
      connector ! Connector.Stop
      if (pool.waitings.nonEmpty && pool.activeCount < pool.maxTotal) {
        val (req, requester) = pool.waitings.dequeue()
        val newConnector = context.actorOf(Connector.props(pool.remote, self))
        doRequest(newConnector, req, requester, pool)
      }
    } else {
      if (pool.waitings.nonEmpty) {
        val (req, requester) = pool.waitings.dequeue()
        doRequest(connector, req, requester, pool)
      } else {
        pool.idles += connector
      }
    }
  }

  private def doRequest(connector: ActorRef, req: Request, requester: ActorRef, pool: ConnectorPool) = {
    pool.activeCount += 1
    ask(connector, req)(Timeout(30, TimeUnit.SECONDS)).onComplete { t =>
      pool.activeCount -= 1
      t match {
        case Success(res) =>
          requester ! res
          release(pool, connector, failure = false)
        case Failure(e: BusyException) =>
          log.error(e, "connector is busy, this should not happen.")
          //here just resend message.
          self.tell(req, requester)
        case Failure(e) =>
          requester ! Status.Failure(e)
          release(pool, connector, failure = true)
      }
    }
  }

  override def preStart(): Unit = {
    super.preStart()
    context.system.scheduler.schedule(5.seconds, 5.seconds, self, CheckDeadline)
  }

  def receive = {
    case req: Request =>
      acquire(req.destination) match {
        case (None, pool) =>
          if (pool.waitings.size < pool.maxWaiting) {
            pool.waitings.enqueue((req, sender()))
          } else {
            sender() ! Status.Failure(new WaitingListFullException(req.destination))
          }
        case (Some(connector), pool) =>
          doRequest(connector, req, sender(), pool)
      }

    case CheckDeadline =>
      val now = Deadline.now
      poolMap.valuesIterator.foreach{pool =>
        while (pool.waitings.nonEmpty && pool.waitings.head._1.deadline <= now) {
          val (request, requester) = pool.waitings.dequeue()
          requester ! Status.Failure(new TimeOutException(request.destination))
        }
      }

    case Connector.DisconnectWhileIdle(remote) =>
      poolMap.get(remote).foreach{pool =>
        val connector = sender()
        pool.idles -= connector
      }
  }

}
