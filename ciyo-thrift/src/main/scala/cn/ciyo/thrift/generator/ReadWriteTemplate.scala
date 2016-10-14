package cn.ciyo.thrift.generator

import cn.ciyo.thrift.ast._
import cn.ciyo.thrift.parser.ParserInternalException

trait ReadWriteTemplate { self: ScalaGenerator =>

  def renderReadValue(sid: SimpleID, t: FieldType): String = {
    t match {
      case t: ListType => readList(sid, t)
      case t: SetType => readSet(sid, t)
      case t: MapType => readMap(sid, t)
      case t: StructType => readStruct(sid, t)
      case t: EnumType => readEnum(sid, t)
      case t: BaseType => readBase(sid, t)
      case t: ReferenceType =>
        throw new ParserInternalException("ReferenceType should have been resolved by now")
    }
  }

  def renderWriteValue(sid: SimpleID, t: FieldType): String = {
    t match {
      case t: ListType => writeList(sid, t)
      case t: SetType => writeSet(sid, t)
      case t: MapType => writeMap(sid, t)
      case t: StructType => writeStruct(sid, t)
      case t: EnumType => writeEnum(sid, t)
      case t: BaseType => writeBase(sid, t)
      case t: ReferenceType =>
        throw new ParserInternalException("ReferenceType should have been resolved by now")
    }
  }

  private def readList(sid: SimpleID, t: ListType) = {
    s"""
       |val _list = _iprot.readListBegin()
       |if (_list.size == 0) {
       |  _iprot.readListEnd()
       |  Nil
       |} else {
       |  val _rv = new mutable$$ArrayBuffer[${genType(t.eltType)}](_list.size)
       |  var _i = 0
       |  while (_i < _list.size) {
       |    _rv += {
       |      ${renderReadValue(sid.append("_element"), t.eltType).intent(3)}
       |    }
       |    _i += 1
       |  }
       |  _iprot.readListEnd()
       |  _rv
       |}
     """.stripMargin
  }

  private def readSet(sid: SimpleID, t: SetType) = {
    s"""
       |val _set = _iprot.readSetBegin()
       |if (_set.size == 0) {
       |  _iprot.readSetEnd()
       |  Set.empty[{{eltType}}]
       |} else {
       |  val _rv = new mutable$$HashSet[${genType(t.eltType)}]
       |  var _i = 0
       |  while (_i < _set.size) {
       |    _rv += {
       |      ${renderReadValue(sid.append("_element"), t.eltType).intent(3)}
       |    }
       |    _i += 1
       |  }
       |  _iprot.readSetEnd()
       |  _rv
       |}
     """.stripMargin
  }

  private def readMap(sid: SimpleID, t: MapType) = {
    s"""
       |val _map = _iprot.readMapBegin()
       |if (_map.size == 0) {
       |  _iprot.readMapEnd()
       |  Map.empty[${genType(t.keyType)}, ${genType(t.valueType)}]
       |} else {
       |  val _rv = new mutable$$HashMap[${genType(t.keyType)}, ${genType(t.valueType)}]
       |  var _i = 0
       |  while (_i < _map.size) {
       |    val _key = {
       |      ${renderReadValue(sid.append("_key"), t.keyType).intent(3)}
       |    }
       |    val _value = {
       |      ${renderReadValue(sid.append("_value"), t.keyType).intent(3)}
       |    }
       |    _rv(_key) = _value
       |    _i += 1
       |  }
       |  _iprot.readMapEnd()
       |  _rv
       |}
     """.stripMargin
  }

  private def readStruct(sid: SimpleID, t: StructType) = {
    s"${genType(t)}.decode(_iprot)"
  }

  private def readEnum(sid: SimpleID, t: EnumType) = {
    s"${genType(t.copy(enum = t.enum.copy(t.enum.sid.toTitleCase)))}.getOrUnknown(_iprot.readI32())"
  }

  private def readBase(sid: SimpleID, t: BaseType) = {
    s"_iprot.${genProtocolReadMethod(t)}()"
  }

  private def genWireConstType(t: FunctionType): String = t match {
    case _: EnumType => "I32"
    case _ => genConstType(t)
  }

  private def writeList(sid: SimpleID, t: ListType) = {
    s"""
       |_oprot.writeListBegin(new TList(TType.${genWireConstType(t.eltType)}, ${genID(sid)}.size))
       |${genID(sid)} match {
       |  case _: IndexedSeq[_] =>
       |    var _i = 0
       |    val _size = ${genID(sid)}.size
       |    while (_i < _size) {
       |      val ${genID(sid.append("_element"))} = ${genID(sid)}(_i)
       |      ${renderWriteValue(sid.append("_element"), t.eltType).intent(3)}
       |      _i += 1
       |    }
       |  case _ =>
       |    ${genID(sid)}.foreach { ${genID(sid.append("_element"))} =>
       |      ${renderWriteValue(sid.append("_element"), t.eltType).intent(3)}
       |    }
       |}
       |_oprot.writeListEnd()
     """.stripMargin
  }

  private def writeSet(sid: SimpleID, t: SetType) = {
    s"""
       |_oprot.writeSetBegin(new TSet(TType.${genWireConstType(t.eltType)}, ${genID(sid)}.size))
       |${genID(sid)}.foreach { ${genID(sid.append("_element"))} =>
       |  ${renderWriteValue(sid.append("_element"), t.eltType).intent(1)}
       |}
       |_oprot.writeSetEnd()
     """.stripMargin
  }

  private def writeMap(sid: SimpleID, t: MapType) = {
    s"""
       |_oprot.writeMapBegin(new TMap(TType.${genWireConstType(t.keyType)}, TType.${genConstType(t.valueType)}, ${genID(sid)}.size))
       |${genID(sid)}.foreach { case (${genID(sid.append("_key"))}, ${genID(sid.append("_value"))}) =>
       |  ${renderWriteValue(sid.append("_key"), t.keyType).intent(1)}
       |  ${renderWriteValue(sid.append("_value"), t.valueType).intent(1)}
       |}
       |_oprot.writeMapEnd()
     """.stripMargin
  }

  private def writeStruct(sid: SimpleID, t: StructType) = {
//    s"${genID(sid)}.write(_oprot)"
    s"${genType(t)}.encode(${genID(sid)}, _oprot)"
  }

  private def writeEnum(sid: SimpleID, t: EnumType) = {
    s"_oprot.writeI32(${genID(sid)}.value)"
  }

  private def writeBase(sid: SimpleID, t: BaseType) = {
    s"_oprot.${genProtocolWriteMethod(t)}(${genID(sid)})"
  }

}
