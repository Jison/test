package cn.ciyo.rpc.client.connection

import java.net.InetSocketAddress

import scala.concurrent.Future

trait ConnectionMediator {
  def request[Req, Res](dest: InetSocketAddress, req: Req): Future[Res]
}
