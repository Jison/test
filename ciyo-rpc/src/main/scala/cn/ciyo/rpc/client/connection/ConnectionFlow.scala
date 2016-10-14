package cn.ciyo.rpc.client.connection

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.stream.TLSProtocol.{SendBytes, SessionBytes, SslTlsInbound, SslTlsOutbound}
import akka.stream.Client
import akka.stream.scaladsl._
import akka.util.ByteString

import scala.concurrent.Future

object ConnectionFlow {

	def apply(address: InetSocketAddress, connectionSettings: ConnectionSettings)(implicit system: ActorSystem): Flow[ByteString, ByteString, Future[Tcp.OutgoingConnection]] = {
		//TODO: 增加一个termination层来实现halfClose?
		val tcp = Tcp().outgoingConnection(address, None, connectionSettings.socketOptions, halfClose = true, connectionSettings.connectingTimeout, connectionSettings.idleTimeout)

		connectionSettings.sslSettings match {
		 case Some(sslSettings) =>
			 val tls = TLS(sslSettings.sslContext, sslSettings.sslConfig, sslSettings.firstSession, Client, hostInfo = Some(address.getHostString -> address.getPort))
			 BidiFlow.fromFlows(Flow[ByteString].map(SendBytes), Flow[SslTlsInbound].collect{case s: SessionBytes => s.bytes}).atop(tls).joinMat(tcp)(Keep.right)
		 case None => tcp
		}
	}

}
