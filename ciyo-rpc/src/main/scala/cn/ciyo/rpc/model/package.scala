package cn.ciyo.rpc

import scala.concurrent.duration.Deadline
import akka.util.ByteString

import scala.util.Try

package object model {

	case class CallMeta(requestId: Option[Long], traceId: Option[Long], deadline: Option[Deadline])

	trait Call[A] {
		def service: ServiceId
		def meta: CallMeta
		def oneWay: Boolean
		def idempotent: Boolean
		def args: A
		def toByteString: ByteString
	}

	trait Result[R] {
		def service: ServiceId
		def meta: CallMeta
		def results: Try[R]
	}

}
