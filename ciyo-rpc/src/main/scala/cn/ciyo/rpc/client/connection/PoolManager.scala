package cn.ciyo.rpc.client.connection

import java.net.InetSocketAddress

import akka.Done
import akka.actor._
import akka.stream.Materializer
import cn.ciyo.rpc.client.connection.PoolFlow.RequestContext

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._

object PoolManager {
	val props = Props[PoolManager].withDeploy(Deploy.local)

	sealed trait PoolStatus
	final case class PoolRunning(ref: ActorRef) extends PoolStatus
	final case class PoolShuttingDown(donePromise: Promise[Done]) extends PoolStatus

	final case class StartPool(address: InetSocketAddress) extends NoSerializationVerificationNeeded
	final case class SendRequest(address: InetSocketAddress, requestContext: RequestContext)
		extends NoSerializationVerificationNeeded
	final case class ShutdownOne(address: InetSocketAddress, donePromise: Promise[Done]) extends NoSerializationVerificationNeeded with DeadLetterSuppression
	final case class ShutdownAll(donePromise: Promise[Done]) extends NoSerializationVerificationNeeded with DeadLetterSuppression

	// INTERNAL API (for testing only)
	final case class GetPoolStatus(address: InetSocketAddress, statusPromise: Promise[Option[PoolStatus]]) extends NoSerializationVerificationNeeded
	final case class GetPoolSize(sizePromise: Promise[Int]) extends NoSerializationVerificationNeeded

}

class PoolManager(implicit fm: Materializer) extends Actor with ActorLogging {

	import PoolManager._

	private[this] var poolStatus = Map[InetSocketAddress, PoolStatus]()
	private[this] var pools = Map[ActorRef, InetSocketAddress]()

	private[this] def startPoolActor(address: InetSocketAddress): ActorRef = {
		if (poolStatus.contains(address)) {
			throw new IllegalStateException(s"pool actor for $address already exists")
		}
		val (poolSettings, connectionSettings) = getConfigForAddress(address)
		val ref = context.actorOf(PoolActor.props(self, address, poolSettings, connectionSettings), PoolActor.name.next())
		poolStatus += address → PoolRunning(ref)
		pools += ref → address
		context.watch(ref)
	}

	private[this] def getConfigForAddress(address: InetSocketAddress): (PoolSettings, ConnectionSettings) = {
		(
			PoolSettings(minConnections =  0, maxConnections =  20, maxOpenRequest =  100, pipeliningLimit =  10, idleTimeout =  1.hour),
			ConnectionSettings(None, scala.collection.immutable.Seq.empty, connectingTimeout = 10.seconds, idleTimeout = 30.seconds)
			)
	}

	def receive = {

		case s @ StartPool(address) ⇒
			poolStatus.get(address) match {
				case Some(PoolRunning(_)) ⇒
				case Some(PoolShuttingDown(shutdownCompletedPromise)) ⇒
					// Pool is being shutdown. When this is done, start the pool again.
					shutdownCompletedPromise.future.onComplete(_ ⇒ self ! s)(context.dispatcher)
				case None ⇒
					startPoolActor(address)
			}

		// Send a request to a pool. If needed, the pool will be started or restarted.
		case s @ SendRequest(address, requestContext) ⇒
			poolStatus.get(address) match {
				case Some(PoolRunning(ref)) ⇒
					ref ! requestContext
				case Some(PoolShuttingDown(donePromise)) ⇒
					// The request will be resent when the pool shutdown is complete (the first
					// request will recreate the pool).
					donePromise.future.foreach(_ ⇒ self ! s)(context.dispatcher)
				case None ⇒
					startPoolActor(address) ! requestContext
			}

		// Shutdown a pool and signal its termination.
		case ShutdownOne(address, donePromise) ⇒
			poolStatus.get(address).foreach {
				case PoolRunning(ref) ⇒
					ref ! PoolActor.Shutdown
					poolStatus += address → PoolShuttingDown(donePromise)
				case PoolShuttingDown(formerPromise) ⇒
					donePromise.tryCompleteWith(formerPromise.future)
				case _ ⇒
					donePromise.trySuccess(Done)
			}

		// Shutdown all known pools and signal their termination.
		case ShutdownAll(donePromise) ⇒
			import context.dispatcher
			def track(remaining: Iterator[Future[Done]]): Unit =
				if (remaining.hasNext) remaining.next().onComplete(_ ⇒ track(remaining))
				else donePromise.trySuccess(Done)
			track(poolStatus.keys.map{ address =>
				val p = Promise[Done]()
				self ! ShutdownOne(address, p)
				p.future
			}.toIterator)

		// When a pool actor terminate, signal its termination and remove it from our maps.
		case Terminated(ref) ⇒
			pools.get(ref).foreach { address ⇒
				poolStatus.get(address) match {
					case Some(PoolRunning(_)) ⇒
						log.error("connection pool for {} has shut down unexpectedly", address)
					case Some(PoolShuttingDown(donePromise)) ⇒
						donePromise.trySuccess(Done)
					case None ⇒
					// This will never happen as poolInterfaces and poolStatus are modified
					// together. If there is no status then there is no gateway to start with.
				}
				poolStatus -= address
				pools -= ref
			}

		// Testing only.
		case GetPoolStatus(gateway, statusPromise) ⇒
			statusPromise.success(poolStatus.get(gateway))

		// Testing only.
		case GetPoolSize(sizePromise) ⇒
			sizePromise.success(poolStatus.size)
	}

}
