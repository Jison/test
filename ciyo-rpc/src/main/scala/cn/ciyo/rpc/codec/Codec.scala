package cn.ciyo.rpc.codec

import akka.util.ByteString

trait Codec[T] {
  def encode(o: T): ByteString
  def decode(bytes: ByteString): T
}

trait CodecFactory {
  def getCodec[T]: Codec[T]
}
