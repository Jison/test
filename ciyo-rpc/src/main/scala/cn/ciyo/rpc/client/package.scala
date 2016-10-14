package cn.ciyo.rpc

import java.net.InetSocketAddress
import akka.util.ByteString
import scala.concurrent.duration.Deadline

package object client {

  trait Request {
    def destination: InetSocketAddress
    def body: ByteString
    def deadline: Deadline
  }

  trait Response {
    def body: ByteString
  }

  case class SimpleRequest(destination: InetSocketAddress, body: ByteString, deadline: Deadline) extends Request
  case class SimpleResponse(body: ByteString) extends Response

}
