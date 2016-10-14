package cn.ciyo.thrift.generator

import java.io.{File, FileOutputStream, OutputStreamWriter}

import cn.ciyo.thrift.ast._
import cn.ciyo.thrift.parser.{ParserInternalException, ResolvedDocument}
import org.scalafmt.{Scalafmt, ScalafmtStyle}

class ScalaGenerator(
  override val resolvedDoc: ResolvedDocument,
  val defaultNamespace: String,
  val experimentFlags: Seq[String]
  ) extends TemplateGenerator(resolvedDoc) with ReadWriteTemplate with ConstsTemplate
  with EnumTemplate with UnionTemplate with StructTemplate with ServiceTemplate {

  val namespaceLanguage = "scala"
  val fileExtension = ".scala"
  var warnOnJavaNamespaceFallback: Boolean = false

  private object ScalaKeywords {
    private[this] val set = Set[String](
      "abstract", "case", "catch", "class", "def", "do", "else", "extends",
      "false", "final", "finally", "for", "forSome", "if", "implicit", "import",
      "lazy", "match", "new", "null", "object", "override", "package", "private",
      "protected", "return", "sealed", "super", "this", "throw", "trait", "try",
      "true", "type", "val", "var", "while", "with", "yield")
    def contains(str: String): Boolean = set.contains(str)
  }

  override protected def writeFile(file: File, fileHeader: String, fileContent: String): Unit = {
    val stream = new FileOutputStream(file)
    val writer = new OutputStreamWriter(stream, "UTF-8")
    val fileBody = fileHeader + fileContent

    val formatedCode = try {
       Scalafmt.format(fileBody, ScalafmtStyle.default120).get
    } catch {
      case e: Exception =>
        println(e.getMessage)
        fileBody
    }

    try {
      writer.write(formatedCode)
    } finally {
      writer.close()
      stream.close()
    }
  }

  // Quote Scala reserved words in ``
  def quoteKeyword(str: String): String =
    if (ScalaKeywords.contains(str))
      "`" + str + "`"
    else
      str

  private[this] def getNamespaceWithWarning(doc: Document): Option[Identifier] =
    doc.namespace("scala") orElse {
      val ns = doc.namespace("java")
      if (ns.isDefined && warnOnJavaNamespaceFallback)
        println("falling back to the java namespace. this will soon be deprecated")
      ns
    }

  override protected def getIncludeNamespace(includeFileName: String): Identifier = {
    val javaNamespace = includeMap.get(includeFileName).flatMap {
      doc: ResolvedDocument => getNamespaceWithWarning(doc.document)
    }
    javaNamespace.getOrElse(SimpleID(defaultNamespace))
  }

  override def getNamespace(doc: Document): Identifier =
    getNamespaceWithWarning(doc).getOrElse(SimpleID(defaultNamespace))

  def genList(list: ListRHS, fieldType: Option[FieldType] = None): String = {
    val listElemType = fieldType.map(_.asInstanceOf[ListType].eltType)
    val code =
      list.elems.map { e =>
        genConstant(e, listElemType)
      }.mkString(", ")
    s"Seq($code)"
  }

  def genSet(set: SetRHS, fieldType: Option[FieldType] = None): String = {
    val setElemType = fieldType.map(_.asInstanceOf[SetType].eltType)
    val code = set.elems.map { e =>
      genConstant(e, setElemType)
    }.mkString(", ")
    s"Set($code)"
  }

  def genMap(map: MapRHS, fieldType: Option[FieldType] = None): String = {
    val mapType = fieldType.map(_.asInstanceOf[MapType])
    val code = map.elems.map { case (k, v) =>
      val key = genConstant(k, mapType.map(_.keyType))
      val value = genConstant(v, mapType.map(_.valueType))
      s"$key -> $value"
    }.mkString(", ")

    s"Map($code)"
  }

  def genEnum(enum: EnumRHS, fieldType: Option[FieldType] = None): String = {
    def getTypeId: Identifier = fieldType.getOrElse(Void) match {
      case n: NamedType => qualifyNamedType(n)
      case _ =>  enum.enum.sid
    }
    genID(enum.value.sid.toTitleCase.addScope(getTypeId.toTitleCase))
  }

  def genStruct(struct: StructRHS, fieldType: Option[FieldType] = None): String = {
    val values = struct.elems
    val fields = values.map { case (f, value) =>
      val v = genConstant(value, Some(f.fieldType))
      genID(f.sid.toCamelCase) + "=" + (if (f.requiredness.isOptional) "Some(" + v + ")" else v)
    }

    val gid = fieldType match {
      case Some(t) => genType(t)
      case None => genID(struct.sid)
    }

    gid + "(" + fields.mkString(", ") + ")"
  }

  def genUnion(union: UnionRHS, fieldType: Option[FieldType] = None): String = {
    val fieldId = genID(union.field.sid.toTitleCase)
    val unionId = fieldType match {
      case Some(t) => genType(t)
      case None => genID(union.sid)
    }
    val rhs = genConstant(union.initializer, Some(union.field.fieldType))
    s"${unionId}.$fieldId($rhs)"
  }

  override def genDefaultValue(fieldType: FieldType): String = {
    val code = fieldType match {
      case TI64 => "0L"
      case MapType(_, _, _) | SetType(_, _) | ListType(_, _) =>
        genType(fieldType) + "()"
      case _ => super.genDefaultValue(fieldType)
    }
    code
  }

  override def genConstant(constant: RHS, fieldType: Option[FieldType] = None): String = {
    (constant, fieldType) match {
      case (IntLiteral(value), Some(TI64)) => value.toString + "L"
      case _ => super.genConstant(constant, fieldType)
    }
  }

  def genType(t: FunctionType): String = {
    val code = t match {
      case Void => "Unit"
      case OnewayVoid => "Unit"
      case TBool => "Boolean"
      case TByte => "Byte"
      case TI16 => "Short"
      case TI32 => "Int"
      case TI64 => "Long"
      case TDouble => "Double"
      case TString => "String"
      case TBinary => "ByteBuffer"
      case MapType(k, v, _) =>
        "Map[" + genType(k) + ", " + genType(v) + "]"
      case SetType(x, _) =>
        "Set[" + genType(x) + "]"
      case ListType(x, _) =>
        "Seq[" + genType(x) + "]"
      case t: NamedType =>
        val id = resolvedDoc.qualifyName(t, namespaceLanguage, defaultNamespace)
        // Named types are capitalized.
        genID(id.toTitleCase)
      case r: ReferenceType =>
        throw new ParserInternalException("ReferenceType should not appear in backend")
    }
    code
  }

  def genPrimitiveType(t: FunctionType): String = genType(t)

  def genFieldType(f: Field): String = {
    val baseType = genType(f.fieldType)
    val code =
      if (f.requiredness.isOptional) {
        "Option[" + baseType + "]"
      } else {
        baseType
      }
    code
  }

  def genFieldParams(fields: Seq[Field], asVal: Boolean = false): String = {
    val code = fields.map { f =>
      val valPrefix = if (asVal) "val " else ""
      val nameAndType = genID(f.sid) + ": " + genFieldType(f)
      val defaultValue =
        genDefaultFieldValue(f).map { d =>
          " = " + d
        }.getOrElse {
          if (f.requiredness.isOptional) " = None"
          else ""
        }

      valPrefix + nameAndType + defaultValue
    }.mkString(", ")
    code
  }

  /*************************Template Helper***************************/

  class TemplateHelper(str: String) {
    def intent(level: Int): String = {
      val intentStr = "  " * level
      val buf = new StringBuilder
      var firstLine = true
      for (line <- str.linesWithSeparators) {
        if (firstLine) {
          buf append line
          firstLine = false
        } else {
          buf append (intentStr + line)
        }
      }
      buf.toString
    }

    def intentAll(level: Int):String = {
      val intentStr = " " * level
      val buf = new StringBuilder
      for (line <- str.linesWithSeparators) {
        buf append (intentStr + line)
      }
      buf.toString
    }
  }

  implicit def stringToTemplateHelper(str: String): TemplateHelper = new TemplateHelper(str)

  def loop[T](s: Seq[T])(f: T => String): String = {
    s.map(f).filter(_.length > 0).mkString("\n")
  }

  def ifShow(condition: => Boolean)(toShow: => String, orShow: => String = "") = {
    if (condition) toShow else orShow
  }
}
