package cn.ciyo.thrift

import org.apache.thrift.TEnum
import org.apache.thrift.protocol.{TProtocol, TType}

import scala.concurrent.{ExecutionContext, Future}

object Thrift {

  trait ThriftCodec[T] {
    def encode(item: T, outProtocol: TProtocol): Unit
    def decode(inProtocol: TProtocol): T
  }

  trait ThriftEnum extends TEnum {
    def value: Int
    def name: String
    def getValue = value
  }

  trait ThriftUnion

  trait ThriftStruct

  trait ThriftException extends Exception

  trait ThriftResponse[Result] {
    def successField: Option[Result]
    def exceptionFields: Iterable[Option[ThriftException]]
    /**
      * Return the first nonempty exception field.
      */
    def firstException(): Option[ThriftException] =
      exceptionFields.collectFirst(ThriftResponse.exceptionIsDefined)
  }

  object ThriftResponse {
    private val exceptionIsDefined: PartialFunction[Option[ThriftException], ThriftException] = {
      case Some(exception) => exception
    }
  }

  trait ThriftFunction {
    /** A struct wrapping method arguments */
    type Args <: ThriftStruct
    /** The successful return type */
    type SuccessType
    /** Contains success or thrift application exceptions */
    type Result <: ThriftResponse[SuccessType]

    def name: String
    def isOneway: Boolean
    def argsCodec: ThriftCodec[Args]
    def resultCodec: ThriftCodec[Result]
    def invoke(args: Args)(implicit ec: ExecutionContext): Future[Result]
  }

  trait ThriftService {
    def name: String = ""
    def functions: Seq[ThriftFunction] = Seq()
  }

  def ttypeToString(byte: Byte): String = {
    // from https://github.com/apache/thrift/blob/master/lib/java/src/org/apache/thrift/protocol/TType.java
    byte match {
      case TType.STOP   => "STOP"
      case TType.VOID   => "VOID"
      case TType.BOOL   => "BOOL"
      case TType.BYTE   => "BYTE"
      case TType.DOUBLE => "DOUBLE"
      case TType.I16    => "I16"
      case TType.I32    => "I32"
      case TType.I64    => "I64"
      case TType.STRING => "STRING"
      case TType.STRUCT => "STRUCT"
      case TType.MAP    => "MAP"
      case TType.SET    => "SET"
      case TType.LIST   => "LIST"
      case TType.ENUM   => "ENUM"
      case _            => "UNKNOWN"
    }
  }

}
