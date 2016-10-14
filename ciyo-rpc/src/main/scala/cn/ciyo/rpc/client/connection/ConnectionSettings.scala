package cn.ciyo.rpc.client.connection

import javax.net.ssl.{SSLContext, SSLParameters}

import akka.io.Inet.SocketOption
import akka.stream.TLSClientAuth
import akka.stream.TLSProtocol.NegotiateNewSession
import com.typesafe.sslconfig.akka.AkkaSSLConfig
import scala.concurrent.duration._

import scala.collection.immutable

case class ConnectionSettings(
	sslSettings: Option[SSLSettings],
	socketOptions: immutable.Seq[SocketOption],
	connectingTimeout: Duration,
	idleTimeout: Duration
)

sealed case class SSLSettings(
	sslContext: SSLContext,
	sslConfig: Option[AkkaSSLConfig] = None,
	enabledCipherSuites: Option[immutable.Seq[String]] = None,
	enabledProtocols: Option[immutable.Seq[String]] = None,
	clientAuth: Option[TLSClientAuth] = None,
	sslParameters: Option[SSLParameters] = None) {
	def firstSession = NegotiateNewSession(enabledCipherSuites, enabledProtocols, clientAuth, sslParameters)
}
