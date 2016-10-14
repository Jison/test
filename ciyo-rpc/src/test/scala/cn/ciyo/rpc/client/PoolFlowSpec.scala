package cn.ciyo.rpc.client

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.event.{Logging, NoLogging}
import akka.io.Inet.SocketOption
import akka.io.Tcp
import akka.stream.ActorMaterializer
import akka.testkit.{ImplicitSender, TestKit}
import akka.util.{ByteString, Timeout}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import akka.stream.scaladsl._
import cn.ciyo.rpc.client.connection.{ConnectionFlow, ConnectionSettings, PoolFlow, PoolSettings}
import cn.ciyo.rpc.client.connection.PoolFlow.RequestContext
import cn.ciyo.rpc.server.{Processor, ServiceServer}
import cn.ciyo.rpc.model.{Call, CallMeta, ServiceId}

import scala.util.{Failure, Success}

class PoolFlowSpec extends TestKit(ActorSystem("test-pool-flow")) with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {

	val serverAddress = new InetSocketAddress(19093)
	val clientAddress = new InetSocketAddress("127.0.0.1", 19093)

	var serverSystem: ActorSystem = _
	var mediator: ActorRef = _
	override def beforeAll = {
		startServer(Processor.echoProps())
		mediator = system.actorOf(ConnectorMediator.props(1, 1, 2))
	}

	var server: ActorRef = _
	def startServer(processorProp: Props) = {
		serverSystem = ActorSystem()
		server = serverSystem.actorOf(ServiceServer.props(ServiceServer.Args(serverAddress), processorProp, testActor))
		expectMsgAnyClassOf(classOf[Tcp.Bound])
	}

	def stopServer() = {
		server = null
		shutdown(serverSystem, verifySystemShutdown = true)
		serverSystem = null
	}

	override def afterAll = {
		TestKit.shutdownActorSystem(system)
		if (serverSystem != null) {
			TestKit.shutdownActorSystem(serverSystem)
		}
	}

	"PoolFlow" should {

		"request" in {
			class TestCall(s: String) extends Call[String] {
				def service: ServiceId = ServiceId("a", "method")
				def meta: CallMeta = CallMeta(None, None, None)
				def oneWay: Boolean = false
				def idempotent: Boolean = true
				def args: String = "a"
				def toByteString: ByteString = ByteString(s)
			}

			implicit val materializer = ActorMaterializer()
			import system.dispatcher
			val socketOptions = scala.collection.immutable.Seq.empty
			val connectionFlow = ConnectionFlow(clientAddress, ConnectionSettings(None, socketOptions, 10.seconds, 10.seconds))(system)
			val log = Logging.getLogger(system, this)
			val poolFlow = PoolFlow(connectionFlow, PoolSettings(1, 3, 10, 1, 10.seconds), log)

			val requests = (1 to 10).map{ i =>
				val p = Promise[ByteString]()
				p.future.onComplete {
					case Success(bs) =>
						println(bs.utf8String)
					case Failure(e) => println(e.getMessage)
				}
				RequestContext(request = new TestCall(s"${i} some one like you"), p)
			}

			Source.fromIterator{() => requests.iterator
			}.via(poolFlow).to(Sink.foreach{ response =>
				response.response match {
					case Success(bs) =>
						println("receive response:" + bs.utf8String)
					case Failure(e) => println("receive response:" + e.getMessage)
				}
				response.rc.responsePromise.complete(response.response)
			}).run()

			val result = Await.result(Future.sequence(requests.map(_.responsePromise.future)), 10.seconds)
			println(result)
		}


	}
}
