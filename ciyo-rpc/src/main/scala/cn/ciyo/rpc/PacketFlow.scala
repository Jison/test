package cn.ciyo.rpc

import akka.NotUsed
import akka.stream.scaladsl._
import akka.util.ByteString
import org.slf4j.LoggerFactory

import scala.collection.immutable.Stream

object PacketFlow {

	final case class Packet(seq: Option[Int], data: ByteString)
	val logger = LoggerFactory.getLogger("PacketFlow")

	def packet(sp: Packet): ByteString = {
		logger.debug("packet")
		val frameSize = 4 + sp.data.length
		sp.seq match {
			case Some(i) => Framing.encodeFrameSize(-frameSize) ++ Framing.encodeFrameSize(i) ++ sp.data
			case None => Framing.encodeFrameSize(frameSize) ++ sp.data
		}
	}

	def unpacket(bs: ByteString): Packet = {
		logger.debug("unpacket")
		val frameSize = Framing.decodeFrameSize(bs)
		if (frameSize < 0) {
			val seq = Framing.decodeFrameSize(bs.drop(4))
			Packet(Some(seq), bs.drop(4))
		} else {
			Packet(None, bs.drop(4))
		}
	}

	def inFlow(): Flow[ByteString, Packet, NotUsed] = {
		Flow.fromFunction[ByteString, Seq[Packet]]((new ReceivePacketing()).apply).mapConcat[Packet](_.asInstanceOf[scala.collection.immutable.Iterable[Packet]])
	}

	def outFlow(): Flow[Packet, ByteString, NotUsed] = {
		Flow.fromFunction(packet)
	}

	private class ReceivePacketing {

		private var buffer = ByteString.empty
		private var currentFrameSize: Int = -1

		def apply(bs: ByteString): Seq[Packet] = {
			buffer ++= bs
			Stream.continually(tryReadPacket()).takeWhile(_.isDefined).map(_.get)
		}

		private def tryReadPacket(): Option[Packet] = {
			if (currentFrameSize <= 0 && buffer.length >= 4) {
				//当前的frame size小于等于0,即未读取frame size
				val readFrameSize = Framing.decodeFrameSize(buffer)
				if (readFrameSize == 0) {
					buffer = buffer.drop(4)
					return None
				}

				currentFrameSize = Math.abs(readFrameSize)
			}

			if (currentFrameSize > 0 && buffer.length >= currentFrameSize + 4) {
				val currentFrame = buffer.slice(0, currentFrameSize + 4)
				buffer = buffer.drop(currentFrameSize + 4)
				currentFrameSize = -1
				Some(unpacket(currentFrame))
			} else {
				None
			}
		}
	}

	object Framing {
		def decodeFrameSize(bytes: ByteString): Int = {
			((bytes(0) & 0xff) << 24) | ((bytes(1) & 0xff) << 16) | ((bytes(2) & 0xff) << 8) | ((bytes(3) & 0xff))
		}

		def encodeFrameSize(size: Int): ByteString = {
			ByteString(0xff & (size >> 24), 0xff & (size >> 16), 0xff & (size >> 8), 0xff & (size))
		}
	}

}
