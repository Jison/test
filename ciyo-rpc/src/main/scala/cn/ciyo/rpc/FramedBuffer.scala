package cn.ciyo.rpc

import akka.util.ByteString

object FramedBuffer {
  def decodeFrameSize(bytes: ByteString): Int = {
    ((bytes(0) & 0xff) << 24) | ((bytes(1) & 0xff) << 16) | ((bytes(2) & 0xff) << 8) | ((bytes(3) & 0xff))
  }

  def encodeFrameSize(size: Int, buf: Array[Byte]) = {
    buf(0) = (0xff & (size >> 24)).toByte
    buf(1) = (0xff & (size >> 16)).toByte
    buf(2) = (0xff & (size >> 8)).toByte
    buf(3) = (0xff & (size)).toByte
  }
}


class FramedBuffer {
  val FrameSizeByteCount = 4
  var buffer: ByteString = ByteString.empty
  var currentFrameSize: Int = -1

  def write(data: ByteString) = {
    buffer ++= data
  }

  @throws[RuntimeException]
  def readAll(): Seq[ByteString] = {
    Stream.continually(readOne()).takeWhile(_.isDefined).map(_.get)
  }

  @throws[RuntimeException]
  def readOne(): Option[ByteString] = {
    if (currentFrameSize <= 0 && buffer.length >= FrameSizeByteCount) {
      //当前的frame size小于等于0,即未读取frame size
      val readFrameSize = FramedBuffer.decodeFrameSize(buffer)
      if (readFrameSize == 0) {
        //frame size不应该少于等于0
        buffer = ByteString.empty
        throw new RuntimeException("Invalid frame size.")
      } else {
        currentFrameSize = Math.abs(readFrameSize)
      }
    }

    if (currentFrameSize > 0 && buffer.length >= currentFrameSize + FrameSizeByteCount) {
      val currentFrameBody = buffer.slice(FrameSizeByteCount, currentFrameSize + FrameSizeByteCount)
      buffer = buffer.drop(currentFrameSize + FrameSizeByteCount)
      currentFrameSize = -1
      Some(currentFrameBody)
    } else {
      None
    }
  }

  def clear() = {
    if (buffer.nonEmpty) {
      buffer = buffer.drop(buffer.size)
    }
  }
}
