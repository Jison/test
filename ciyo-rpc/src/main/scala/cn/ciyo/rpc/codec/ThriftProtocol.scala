package cn.ciyo.rpc.codec

import java.nio.ByteBuffer

import org.apache.thrift.protocol._

class ThriftProtocol extends TProtocol(null) {
	override def readBool(): Boolean = ???

	override def writeMapEnd(): Unit = ???

	override def readSetBegin(): TSet = ???

	override def writeFieldEnd(): Unit = ???

	override def writeString(str: String): Unit = ???

	override def readByte(): Byte = ???

	override def readStructBegin(): TStruct = ???

	override def writeMessageBegin(message: TMessage): Unit = ???

	override def writeListBegin(list: TList): Unit = ???

	override def writeI32(i32: Int): Unit = ???

	override def readStructEnd(): Unit = ???

	override def writeDouble(dub: Double): Unit = ???

	override def readListEnd(): Unit = ???

	override def writeBinary(buf: ByteBuffer): Unit = ???

	override def readI32(): Int = ???

	override def writeSetEnd(): Unit = ???

	override def writeMapBegin(map: TMap): Unit = ???

	override def writeFieldBegin(field: TField): Unit = ???

	override def readI64(): Long = ???

	override def writeSetBegin(set: TSet): Unit = ???

	override def writeI64(i64: Long): Unit = ???

	override def writeI16(i16: Short): Unit = ???

	override def readI16(): Short = ???

	override def writeMessageEnd(): Unit = ???

	override def readMessageBegin(): TMessage = ???

	override def readFieldBegin(): TField = ???

	override def writeStructEnd(): Unit = ???

	override def readListBegin(): TList = ???

	override def readMapEnd(): Unit = ???

	override def writeBool(b: Boolean): Unit = ???

	override def readFieldEnd(): Unit = ???

	override def readString(): String = ???

	override def readMessageEnd(): Unit = ???

	override def readDouble(): Double = ???

	override def writeByte(b: Byte): Unit = ???

	override def writeFieldStop(): Unit = ???

	override def readBinary(): ByteBuffer = ???

	override def writeStructBegin(struct: TStruct): Unit = ???

	override def readSetEnd(): Unit = ???

	override def readMapBegin(): TMap = ???

	override def writeListEnd(): Unit = ???
}
