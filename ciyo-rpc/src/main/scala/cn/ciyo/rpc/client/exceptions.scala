package cn.ciyo.rpc.client

import java.net.InetSocketAddress

import scala.util.control.NoStackTrace

class TimeOutException(remote: InetSocketAddress) extends Exception(s"timeout to receive response from remote: ${remote.toString}") with NoStackTrace

class BusyException(remote: InetSocketAddress) extends Exception(s"the connection is being used of address:${remote.toString}") with NoStackTrace

class DisconnectedException(remote: InetSocketAddress) extends Exception(s"underlying connection close of address:${remote.toString}") with NoStackTrace

class TransportException(message: String) extends Exception(message)
