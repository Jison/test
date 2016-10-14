package cn.ciyo.thrift.generator

import cn.ciyo.thrift.ast._

trait UnionTemplate { self: ScalaGenerator =>

  def renderUnion(namespace: Identifier, union: UnionDef): String = {
    val UnionName = genID(union.sid.toTitleCase)
    val UnionNameForWire = union.originalName

    s"""
       |package ${genID(namespace)}
       |
       |import cn.ciyo.thrift.Thrift._
       |import org.apache.thrift.protocol._
       |import scala.collection.immutable.{Map => immutable$$Map}
       |import scala.collection.mutable.{ ArrayBuffer => mutable$$ArrayBuffer, Buffer => mutable$$Buffer, HashMap => mutable$$HashMap, HashSet => mutable$$HashSet}
       |import scala.collection.{Map, Set}
       |
       |@javax.annotation.Generated(value = Array("cn.ciyo.thrift.Compiler"))
       |sealed trait ${UnionName} {
       |  def write(_oprot: TProtocol): Unit
       |}
       |
       |${union.docstring.getOrElse("")}
       |@javax.annotation.Generated(value = Array("cn.ciyo.thrift.Compiler"))
       |object ${UnionName} {
       |  val Union = new TStruct("${UnionNameForWire}")
       |  ${loop(union.fields)(renderField).intent(1)}
       |
       |  def encode(_item: ${UnionName}, _oprot: TProtocol): Unit = _item.write(_oprot)
       |
       |  def decode(_iprot: TProtocol): ${UnionName} = {
       |    var _result: ${UnionName} = null
       |    _iprot.readStructBegin()
       |    val _field = _iprot.readFieldBegin()
       |    _field.id match {
       |      ${loop(union.fields)(renderReadField(UnionName, _)).intent(3)}
       |      case _ =>
       |        TProtocolUtil.skip(_iprot, _field.`type`)
       |    }
       |    if (_field.`type` != TType.STOP) {
       |      _iprot.readFieldEnd()
       |      var _done = false
       |      var _moreThanOne = false
       |      while (!_done) {
       |        val _field = _iprot.readFieldBegin()
       |        if (_field.`type` == TType.STOP)
       |          _done = true
       |        else {
       |          _moreThanOne = true
       |          TProtocolUtil.skip(_iprot, _field.`type`)
       |          _iprot.readFieldEnd()
       |        }
       |      }
       |      if (_moreThanOne) {
       |        _iprot.readStructEnd()
       |        throw new TProtocolException("Cannot read a TUnion with more than one set value!")
       |      }
       |    }
       |    _iprot.readStructEnd()
       |    if (_result == null)
       |      throw new TProtocolException("Cannot read a TUnion with no set value!")
       |    _result
       |  }
       |
       |
       |  def apply(_iprot: TProtocol): ${UnionName} = decode(_iprot)
       |
       |  ${loop(union.fields)(renderFieldClass(UnionName, _)).intent(1)}
       |}
     """.stripMargin
  }

  private def renderField(field: Field): String = {
    val fieldConstType = genConstType(field.fieldType)
    val fieldIndex = field.index.toString

    s"""val ${genID(field.sid.toTitleCase.append("Field"))} = new TField("${field.originalName}", TType.${fieldConstType}, ${fieldIndex})""" +
      (if (field.fieldType.isInstanceOf[EnumType]) {
        s"""
           |
           |private[this] val ${fieldConstType}I32 = new TField("${field.originalName}", TType.I32, ${fieldIndex})
         """.stripMargin
      } else "")
  }

  private def renderReadField(UnionName: String, field: Field) = {
    val isEnum = field.fieldType.isInstanceOf[EnumType]

    s"""
       |case ${field.index.toString} => // ${genID(field.sid)}
       |  _field.`type` match {
       |    case ${if(isEnum) "TType.I32 | TType.ENUM" else s"TType.${genConstType(field.fieldType)}"} =>
       |      _result = ${UnionName}.${genID(field.sid.toTitleCase)}({
       |        ${renderReadValue(field.sid.append("_item"), field.fieldType)}
       |      })
       |    case _ => TProtocolUtil.skip(_iprot, _field.`type`)
       |  }
     """.stripMargin
  }

  private def renderFieldClass(UnionName: String, field: Field) = {
    val setupDefaultValue = genDefaultFieldValue(field) match {
      case Some(v) => " = " + v
      case None => ""
    }

    val checkIsNull = if (isPrimitive(field.fieldType))
      s"""if (value == null) throw new TProtocolException("Cannot write a TUnion with no set value!")"""
      else ""

    s"""
       |case class ${genID(field.sid.toTitleCase)}(value: ${genType(field.fieldType)}${setupDefaultValue}) extends ${UnionName} {
       |  override def write(_oprot: TProtocol): Unit = {
       |    ${checkIsNull}
       |    _oprot.writeStructBegin(Union)
       |    ${renderWriteValue(SimpleID("value", None), field.fieldType)}
       |    _oprot.writeFieldStop()
       |    _oprot.writeStructEnd()
       |  }
       |}
     """.stripMargin
  }

}
