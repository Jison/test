package cn.ciyo.rpc

import akka.util.ByteString
import org.apache.thrift.transport.TTransport

trait ByteStringTransport extends TTransport {
  def getByteString: ByteString
  def resetByteString(aByteString: ByteString): Unit
}


object InputByteStringTransport {
  def apply(aByteString: ByteString) = new InputByteStringTransport(aByteString)
}

class InputByteStringTransport(aByteString: ByteString) extends ByteStringTransport {

  private var byteString = aByteString
  private var byteIterator = byteString.iterator

  def resetByteString(aByteString: ByteString): Unit = {
    byteString = aByteString
    byteIterator = byteString.iterator
  }

  def clear() = {
    byteString = null
    byteIterator = null
  }

  def getByteString = byteString

  override def write(buf: Array[Byte], off: Int, len: Int): Unit = {
    throw new UnsupportedOperationException("No writing allowed!")
  }

  override def isOpen: Boolean = true

  override def close(): Unit = {}

  override def read(buf: Array[Byte], off: Int, len: Int): Int = {
    val beforeLen = byteIterator.len
    byteIterator.copyToArray(buf, off, len)
    beforeLen - byteIterator.len
  }

  override def open(): Unit = {}

//  private lazy val buffer = byteString.toArray[Byte]
//  override def getBuffer: Array[Byte] = buffer
//
//  override def getBufferPosition: Int = pos
//
//  override def getBytesRemainingInBuffer: Int = endPos - pos
//
//  override def consumeBuffer(len: Int): Unit = pos += len
}

object OutputByteStringTransport {
  def apply(onFlush: ByteString => Unit) = new OutputByteStringTransport(onFlush)
}

class OutputByteStringTransport(onFlush: ByteString => Unit) extends ByteStringTransport {

  val bsBuilder = ByteString.createBuilder

  def clear() = {
    bsBuilder.clear()
  }

  def getByteString = bsBuilder.result()

  def resetByteString(bs: ByteString) = {
    bsBuilder.clear()
    bsBuilder.append(bs)
  }

  override def write(buf: Array[Byte], off: Int, len: Int): Unit = {
    bsBuilder.putBytes(buf, off, len)
  }

  override def isOpen: Boolean = true

  override def close(): Unit = {}

  override def read(buf: Array[Byte], off: Int, len: Int): Int = 0

  override def open(): Unit = bsBuilder.sizeHint(32)

  override def flush(): Unit = onFlush(bsBuilder.result())
}
