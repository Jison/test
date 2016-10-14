package cn.ciyo.thrift.generator

import cn.ciyo.thrift.ast._

trait StructTemplate { self: ScalaGenerator =>

  private class FieldWrapper(field: Field) {
    def required = field.requiredness.isRequired
    def optional = field.requiredness.isOptional
    def isEnum = field.fieldType.isInstanceOf[EnumType]
    def nullable = isNullableType(field.fieldType, field.requiredness.isOptional)
    def gName = genID(field.sid)
    def gGotName = genID(field.sid.prepend("_got_"))
    def gConstType = genConstType(field.fieldType)
    def gType = genType(field.fieldType)
    def gId = field.index.toString
    def gFieldName = genID(field.sid.toTitleCase.append("Field"))
    def gTypeWithOptional = if (field.optional) s"scala.Option[${gType}]" else gType
    def gReadFieldValueName = genID(field.sid.toTitleCase.prepend("read").append("Value"))
    def gWriteFieldName = genID(field.sid.toTitleCase.prepend("write").append("Field"))
    def gWriteFieldValueName = genID(field.sid.toTitleCase.prepend("write").append("Value"))
  }

  implicit private def wrapField(field: Field): FieldWrapper = new FieldWrapper(field)

  def getSuccessType(result: FunctionResultDef): String = result.success match {
    case Some(field) => genType(field.fieldType)
    case None => "Unit"
  }

  def getSuccessValue(result: FunctionResultDef): String = result.success match {
    case Some(field) => field.gName
    case None => "Some(Unit)"
  }

  def getExceptionFields(result: FunctionResultDef): String = {
    val exceptions = result.exceptions.map { field: Field => genID(field.sid) }.mkString(", ")
    s"Seq($exceptions)"
  }

  def renderStruct(namespace: Option[Identifier], struct: StructLikeDef): String = {
    val StructName = genID(struct.sid.toTitleCase)
    val StructNameForWire = struct.originalName
    val InstanceClassName = StructName
    val ParentType = struct match {
      case e: ExceptionDef => "ThriftException"
      case u: UnionDef => "ThriftUnion"
      case result: FunctionResultDef =>
        val resultType = getSuccessType(result)
        s"ThriftResponse[$resultType]"
      case _ => "ThriftStruct"
    }

    s"""
       |${renderHeader(namespace, struct)}
       |
       |${struct.docstring.getOrElse("")}
       |object ${StructName} extends ThriftCodec[${StructName}] {
       |  val Struct = new TStruct("${StructNameForWire}")
       |  ${loop(struct.fields)(renderStructField).intent(1)}
       |
       |  /**
       |   * Checks that all required fields are non-null.
       |   */
       |  def validate(_item: ${StructName}): Unit = {
       |    ${loop(struct.fields)(renderValidField("_item")).intent(2)}
       |  }
       |
       |  def encode(_item: ${StructName}, _oprot: TProtocol): Unit = {
       |    validate(_item)
       |    _oprot.writeStructBegin(Struct)
       |    ${loop(struct.fields)(renderWriteField(StructName, "_item", _)).intent(2)}
       |    _oprot.writeFieldStop()
       |    _oprot.writeStructEnd()
       |  }
       |
       |  def decode(_iprot: TProtocol): ${StructName} = {
       |    ${loop(struct.fields)(renderDecodeField).intent(2)}
       |
       |    var _done = false
       |
       |    _iprot.readStructBegin()
       |    while (!_done) {
       |      val _field = _iprot.readFieldBegin()
       |      if (_field.`type` == TType.STOP) {
       |        _done = true
       |      } else {
       |        _field.id match {
       |          ${loop(struct.fields){f => s"case ${f.index.toString} => ${renderReadField(f)}"}.intent(4)}
       |          case _ => //do nothing.
       |        }
       |        _iprot.readFieldEnd()
       |      }
       |    }
       |    _iprot.readStructEnd()
       |
       |    ${loop(struct.fields){f => if(f.required)
         s"""
            |if (!${f.gGotName}) throw new TProtocolException("Required field '${f.gName}' was not found in serialized data for struct ${StructName}")
          """.stripMargin else ""}}
       |    new ${InstanceClassName}(${struct.fields.map(_.gName).mkString(", ")})
       |  }
       |
       |  def apply(${struct.fields.map(renderAssignFieldValue(_)).mkString(", ")}): ${StructName} =
       |    new ${InstanceClassName}(${struct.fields.map(_.gName).mkString(", ")})
       |
       |  ${renderUnapply(struct).intent(1)}
       |
       |  ${renderReadWriteFields(struct).intent(1)}
       |}
       |
       |class ${StructName}(${struct.fields.map{f => s"val ${renderAssignFieldValue(f)}"}.mkString(", ")}) extends ${ParentType} with ${productN(struct.fields)} with cn.ciyo.thrift.ProductMixin with java.io.Serializable {
       |  ${loop(struct.fields.zipWithIndex){a => val (f, i) = a; s"def _${i + 1} = ${f.gName}"}.intent(1)}
       |
       |  ${renderResponseFields(struct).intent(1)}
       |  ${renderExceptionFields(struct).intent(1)}
       |
       |  def copy(${struct.fields.map{f => s"${f.gName}: ${f.gTypeWithOptional} = this.${f.gName}"}.mkString(", ")}): ${StructName} = new ${InstanceClassName}(${struct.fields.map{f => f.gName}.mkString(", ")})
       |
       |  override def canEqual(other: Any): Boolean = other.isInstanceOf[${StructName}]
       |}
     """.stripMargin
  }

  private def renderHeader(namespace: Option[Identifier], struct: StructLikeDef) = {
    namespace match {
      case None => ""
      case Some(ns) if !(struct.isInstanceOf[FunctionArgsDef] || struct.isInstanceOf[FunctionResultDef])=>
        s"""
           |package ${genID(ns)}
           |
           |import cn.ciyo.thrift.Thrift._
           |import org.apache.thrift.protocol._
           |import scala.collection.immutable.{Map => immutable$$Map}
           |import scala.collection.mutable.{
           |  ArrayBuffer => mutable$$ArrayBuffer, Buffer => mutable$$Buffer,
           |  HashMap => mutable$$HashMap, HashSet => mutable$$HashSet}
           |import scala.collection.{Map, Set}
           |
         """.stripMargin
    }
  }

  private def renderStructField(field: Field) = {
    s"""
       |val ${field.gFieldName} = new TField("${field.originalName}", TType.${field.gConstType}, ${field.gId})
     """.stripMargin +
    (if (field.isEnum) {
      s"""
         |
         |val ${field.gFieldName}I32 = new TField("${field.originalName}", TType.I32, ${field.gId})
       """.stripMargin
    } else "")
  }

  private def renderValidField(itemName: String)(field: Field) = {
    if (field.required && field.nullable) {
      s"""if (${itemName}.${field.gName} == null) throw new TProtocolException("Required field ${field.gName} cannot be null")"""
    } else {
      ""
    }
  }

  private def renderDecodeField(field: Field) = {
    val r = if (field.optional) {
      s"var ${field.gName}: Option[${field.gType}] = None"
    } else {
      s"var ${field.gName}: ${field.gType} = ${genDefaultReadValue(field)}"
    }

    if (field.required) {
      r + "\n" + s"var ${field.gGotName} = false"
    } else r
  }

  private def renderReadField(field: Field) = {
    val matchType = if (field.isEnum) "TType.I32 | TType.ENUM" else s"TType.${field.gConstType}"
    val expectedType = if (field.isEnum) "TType.ENUM" else s"TType.${field.gConstType}"
    val value = if (field.optional) s"Some(${field.gReadFieldValueName}(_iprot))" else
      s"${field.gReadFieldValueName}(_iprot)"

    s"""
       |_field.`type` match {
       |  case ${matchType} =>
       |    ${field.gName} = ${value}
       |    ${if (field.required) s"${field.gGotName} = true" else "" }
       |  case _actualType =>
       |    val _expectedType = ${expectedType}
       |    throw new TProtocolException(
       |      "Received wrong type for field '${field.gName}' (expected=%s, actual=%s).".format(
       |        ttypeToString(_expectedType),
       |        ttypeToString(_actualType)
       |      )
       |    )
       |}
     """.stripMargin
  }

  private def renderAssignFieldValue(field: Field) = {
    if (field.optional) {
      s"${field.gName}: scala.Option[${field.gType}] = scala.None"
    } else {
      s"${field.gName}: ${field.gType}" + (genDefaultFieldValue(field) match {
        case Some(d) => " = " + d
        case None => ""
      })
    }
  }

  private def renderUnapply(struct: StructLikeDef) = {
    val StructName = genID(struct.sid.toTitleCase)
    if (struct.fields.length == 0) {
      s"def unapply(_item: ${StructName}): Boolean = true"
    } else if (struct.fields.length == 1) {
      val field = struct.fields.head
      s"def unapply(_item: ${StructName}): scala.Option[${field.gTypeWithOptional}] = scala.Some(_item.${field.gName})"
    } else if (struct.fields.length > 1 && struct.fields.length < 22) {
      val product = productN(struct.fields)
      s"def unapply(_item: ${StructName}): scala.Option[${product}] = scala.Some(_item)"
    } else {
      ""
    }
  }

  /**
    * Returns a String "scala.Product${N}[Type1, Type2, ...]" or "scala.Product".
    */
  private def productN(fields: Seq[Field]): String = {
    val arity = fields.length
    if (arity >= 1 && arity <= 22) {
      val fieldTypes = fields.map { f =>
        genFieldType(f)
      }.mkString(", ")
      s"scala.Product$arity[$fieldTypes]"
    } else {
      "cn.ciyo.thrift.Product0"
    }
  }

  private def renderReadWriteFields(struct: StructLikeDef) = {
    loop(struct.fields){f =>
      s"""
         |@inline private def ${f.gReadFieldValueName}(_iprot: TProtocol): ${f.gType} = {
         |  ${renderReadValue(f.sid.append("_item"), f.fieldType)}
         |}
         |
         |@inline private def ${f.gWriteFieldName}(${genID(f.sid.append("_item"))}: ${f.gType}, _oprot: TProtocol): Unit = {
         |  _oprot.writeFieldBegin(${f.gFieldName}${ifShow(f.isEnum)("I32")})
         |  ${f.gWriteFieldValueName}(${genID(f.sid.append("_item"))}, _oprot)
         |  _oprot.writeFieldEnd()
         |}
         |
         |@inline private def ${f.gWriteFieldValueName}(${genID(f.sid.append("_item"))}: ${f.gType}, _oprot: TProtocol): Unit = {
         |  ${renderWriteValue(f.sid.append("_item"), f.fieldType)}
         |}
       """.stripMargin}
  }

  private def renderWriteField(StructName: String, instanceVarableName: String, field: Field) = {
    if (field.optional) {
      s"if (${instanceVarableName}.${field.gName}.isDefined) ${StructName}.${field.gWriteFieldName}(${instanceVarableName}.${field.gName}.get, _oprot)"
    } else if (field.nullable) {
      s"if (${instanceVarableName}.${field.gName} ne null) ${StructName}.${field.gWriteFieldName}(${instanceVarableName}.${field.gName}, _oprot)"
    } else {
      s"${StructName}.${field.gWriteFieldName}(${instanceVarableName}.${field.gName}, _oprot)"
    }
  }

  private def renderResponseFields(struct: StructLikeDef) = {
    struct match {
      case r: FunctionResultDef =>
        s"""
           |def successField: Option[${getSuccessType(r)}] = ${getSuccessValue(r)}
           |def exceptionFields: Iterable[Option[ThriftException]] = ${getExceptionFields(r)}
         """.stripMargin
      case _ => ""
    }
  }

  private def renderExceptionFields(struct: StructLikeDef) = {
    struct match {
      case e: ExceptionDef =>
        val msgField: Option[Field] = struct.fields.find { field =>
          // 1st choice: find a field called message
          field.sid.name == "message"
        }.orElse {
          // 2nd choice: the first string field
          struct.fields.find {
            field => field.fieldType == TString
          }
        }

        msgField match {
          case Some(f) =>
            s"""
               |override def getMessage: String = String.valueOf(${f.gName})
             """.stripMargin
          case _ => ""
        }
      case _ => ""
    }
  }

}
