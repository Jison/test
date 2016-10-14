package cn.ciyo.rpc.client

import cn.ciyo.rpc.codec.CodecFactory
import scala.concurrent.Future

trait ClientMediator {
  def apply[Req, Res](serviceName: String, req: Req, codecFactory: CodecFactory): Future[Res]
}