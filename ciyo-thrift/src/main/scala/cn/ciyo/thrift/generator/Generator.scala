package cn.ciyo.thrift.generator

import java.io.{File, FileOutputStream, OutputStreamWriter}

import cn.ciyo.thrift.ast._
import cn.ciyo.thrift.parser.{ParserInternalException, ResolvedDocument}

import scala.collection.mutable

abstract class Generator(val resolvedDoc: ResolvedDocument) {

  def apply(outputPath: File, dryRun: Boolean = false): Iterable[File]

  def namespaceLanguage: String

  def includeMap: Map[String, ResolvedDocument] = resolvedDoc.resolver.includeMap
}


abstract class TemplateGenerator(resolvedDoc: ResolvedDocument) extends Generator(resolvedDoc) {

  /**
    * Map from included file names to the namespaces defined in those files.
    */
  val defaultNamespace: String
  val experimentFlags: Seq[String]

  /******************** helper functions ************************/
  protected def namespacedFolder(destFolder: File, namespace: String, dryRun: Boolean): File = {
    val file = new File(destFolder, namespace.replace('.', File.separatorChar))
    if (!dryRun) file.mkdirs()
    file
  }

  protected def getIncludeNamespace(includeFileName: String): Identifier = {
    val javaNamespace = includeMap.get(includeFileName).flatMap {
      doc: ResolvedDocument => doc.document.namespace("java")
    }
    javaNamespace.getOrElse(SimpleID(defaultNamespace))
  }

  def normalizeCase[N <: Node](node: N): N = {
    (node match {
      case d: Document =>
        d.copy(defs = d.defs.map(normalizeCase))
      case id: Identifier => id.toTitleCase
      case e: EnumRHS =>
        e.copy(normalizeCase(e.enum), normalizeCase(e.value))
      case f: Field =>
        f.copy(
          sid = f.sid.toCamelCase,
          default = f.default.map(normalizeCase))
      case f: Function =>
        f.copy(
          funcName = f.funcName.toCamelCase,
          args = f.args.map(normalizeCase),
          throws = f.throws.map(normalizeCase))
      case c: ConstDef =>
        c.copy(value = normalizeCase(c.value))
      case e: EnumDef =>
        e.copy(values = e.values.map(normalizeCase))
      case e: EnumFieldDef =>
        e.copy(sid = e.sid.toTitleCase)
      case s: StructDef =>
        s.copy(fields = s.fields.map(normalizeCase))
      case f: FunctionArgsDef =>
        f.copy(fields = f.fields.map(normalizeCase))
      case f: FunctionResultDef =>
        f.copy(success = f.success.map(normalizeCase), exceptions = f.exceptions.map(normalizeCase))
      case e: ExceptionDef =>
        e.copy(fields = e.fields.map(normalizeCase))
      case s: ServiceDef =>
        s.copy(functions = s.functions.map(normalizeCase))
      case n => n
    }).asInstanceOf[N]
  }

  def getNamespace(doc: Document): Identifier =
    doc.namespace("java") getOrElse (SimpleID(defaultNamespace))

  def quote(str: String) = "\"" + str + "\""
  def quoteKeyword(str: String): String

  def isNullableType(t: FieldType, isOptional: Boolean = false) = {
    !isOptional && (
      t match {
        case TBool | TByte | TI16 | TI32 | TI64 | TDouble => false
        case _ => true
      }
      )
  }

  def getServiceParentID(parent: ServiceParent): Identifier = {
    val identifier: Identifier = parent.filename match {
      case Some(scope) => parent.sid.addScope(getIncludeNamespace(scope.name))
      case None => parent.sid
    }
    identifier.toTitleCase
  }

  def isPrimitive(t: FunctionType): Boolean = {
    t match {
      case Void | TBool | TByte | TI16 | TI32 | TI64 | TDouble => true
      case _ => false
    }
  }

  def isLazyReadEnabled(t: FunctionType, optional: Boolean): Boolean = {
    t match {
      case TString => true
      case Void | TBool | TByte | TI16 | TI32 | TI64 | TDouble => optional
      case _ => false
    }
  }

  protected def writeFile(file: File, fileHeader: String, fileContent: String): Unit = {
    val stream = new FileOutputStream(file)
    val writer = new OutputStreamWriter(stream, "UTF-8")
    val fileBody = fileHeader + fileContent

    try {
      writer.write(fileBody)
    } finally {
      writer.close()
      stream.close()
    }
  }

  // methods that convert AST nodes to CodeFragment
  def genID(data: Identifier): String = data match {
    case SimpleID(name, _) => quoteKeyword(name)
    case QualifiedID(names) => names.map(quoteKeyword).mkString(".")
  }

  // Add namespace if id is unqualified.
  def genQualifiedID(id: Identifier, namespace: Identifier): String =
    id match {
      case sid: SimpleID => genID(sid.addScope(namespace))
      case qid: QualifiedID => genID(qid)
    }

  def genConstant(constant: RHS, fieldType: Option[FieldType] = None): String = {
    constant match {
      case NullLiteral => "null"
      case StringLiteral(value) => quote(value)
      case DoubleLiteral(value) => value.toString
      case IntLiteral(value) => value.toString
      case BoolLiteral(value) => value.toString
      case c@ListRHS(_) => genList(c, fieldType)
      case c@SetRHS(_) => genSet(c, fieldType)
      case c@MapRHS(_) => genMap(c, fieldType)
      case c: EnumRHS => genEnum(c, fieldType)
      case iv@IdRHS(id) => genID(id)
      case s: StructRHS => genStruct(s, fieldType)
      case u: UnionRHS => genUnion(u, fieldType)
    }
  }

  def genList(list: ListRHS, fieldType: Option[FieldType] = None): String

  def genSet(set: SetRHS, fieldType: Option[FieldType] = None): String

  def genMap(map: MapRHS, fieldType: Option[FieldType] = None): String

  def genEnum(enum: EnumRHS, fieldType: Option[FieldType] = None): String

  def genStruct(struct: StructRHS, fieldType: Option[FieldType] = None): String

  def genUnion(union: UnionRHS, fieldType: Option[FieldType] = None): String

  /**
    * The default value for the specified type and mutability.
    */
  def genDefaultValue(fieldType: FieldType): String = {
    val code = fieldType match {
      case TBool => "false"
      case TByte | TI16 | TI32 | TI64 => "0"
      case TDouble => "0.0"
      case _ => "null"
    }
    code
  }

  def genDefaultFieldValue(f: Field): Option[String] = {
    if (f.requiredness.isOptional) {
      None
    } else {
      f.default.map(genConstant(_, Some(f.fieldType))).orElse {
        if (f.fieldType.isInstanceOf[ContainerType]) {
          Some(genDefaultValue(f.fieldType))
        } else {
          None
        }
      }
    }
  }

  def genDefaultReadValue(f: Field): String =
    genDefaultFieldValue(f).getOrElse(genDefaultValue(f.fieldType))

  def genConstType(t: FunctionType): String = {
    val code = t match {
      case Void => "VOID"
      case TBool => "BOOL"
      case TByte => "BYTE"
      case TDouble => "DOUBLE"
      case TI16 => "I16"
      case TI32 => "I32"
      case TI64 => "I64"
      case TString => "STRING"
      case TBinary => "STRING" // thrift's idea of "string" is based on old broken c++ semantics.
      case StructType(_, _) => "STRUCT"
      case EnumType(_, _) => "ENUM"
      case MapType(_, _, _) => "MAP"
      case SetType(_, _) => "SET"
      case ListType(_, _) => "LIST"
      case x => throw new InternalError("constType#" + t)
    }
    code
  }

  /**
    * When a named type is imported via include statement, we need to
    * qualify it using its full namespace
    */
  def qualifyNamedType(t: NamedType, namespace: Option[Identifier] = None): Identifier =
    t.scopePrefix match {
      case Some(scope) => t.sid.addScope(getIncludeNamespace(scope.name))
      case None if namespace.isDefined => t.sid.addScope(namespace.get)
      case None => t.sid
    }

  def genProtocolReadMethod(t: FunctionType): String = {
    val code = t match {
      case TBool => "readBool"
      case TByte => "readByte"
      case TI16 => "readI16"
      case TI32 => "readI32"
      case TI64 => "readI64"
      case TDouble => "readDouble"
      case TString => "readString"
      case TBinary => "readBinary"
      case x => throw new ParserInternalException("protocolReadMethod#" + t)
    }
    code
  }

  def genOffsetSkipProtocolMethod(t: FunctionType): String = {
    val code = t match {
      case TBool => "offsetSkipBool"
      case TByte => "offsetSkipByte"
      case TI16 => "offsetSkipI16"
      case TI32 => "offsetSkipI32"
      case TI64 => "offsetSkipI64"
      case TDouble => "offsetSkipDouble"
      case TString => "offsetSkipString"
      case TBinary => "offsetSkipBinary"
      case x => s"""Invalid type passed($x) for genOffsetSkipProtocolMethod method. Compile will fail here."""
    }
    code
  }

  def genDecodeProtocolMethod(t: FunctionType): String = {
    val code = t match {
      case TBool => "decodeBool"
      case TByte => "decodeByte"
      case TI16 => "decodeI16"
      case TI32 => "decodeI32"
      case TI64 => "decodeI64"
      case TDouble => "decodeDouble"
      case TString => "decodeString"
      case TBinary => "decodeBinary"
      case x => s"""Invalid type passed ($x) for genDecodeProtocolMethod method. Compile will fail here."""
    }
    code
  }

  def genProtocolWriteMethod(t: FunctionType): String = {
    val code = t match {
      case TBool => "writeBool"
      case TByte => "writeByte"
      case TI16 => "writeI16"
      case TI32 => "writeI32"
      case TI64 => "writeI64"
      case TDouble => "writeDouble"
      case TString => "writeString"
      case TBinary => "writeBinary"
      case x => throw new ParserInternalException("protocolWriteMethod#" + t)
    }
    code
  }

  def genType(t: FunctionType): String

  def genPrimitiveType(t: FunctionType): String

  def genFieldType(f: Field): String

  def genFieldParams(fields: Seq[Field], asVal: Boolean = false): String

//  def templates: Any
  def fileExtension: String

  def renderConsts(namespace: Identifier, consts: Seq[ConstDef]): String
  def renderEnum(namespace: Identifier, enum: EnumDef): String
  def renderUnion(namespace: Identifier, union: UnionDef): String
  def renderStruct(namespace: Option[Identifier], struct: StructLikeDef): String
  def renderService(namespace: Identifier, service: ServiceDef): String

  def apply(outputPath: File, dryRun: Boolean = false): Iterable[File] = {
    val generatedFiles = new mutable.ListBuffer[File]
    val doc = normalizeCase(resolvedDoc.document)
    val namespace = getNamespace(resolvedDoc.document)
    val packageDir = namespacedFolder(outputPath, namespace.fullName, dryRun)
    val includes = doc.headers.collect {
      case x@Include(_, _) => x
    }

    if (doc.consts.nonEmpty) {
      val file = new File(packageDir, "Constants" + fileExtension)
      if (!dryRun) {
        writeFile(file, "", renderConsts(namespace, doc.consts))
      }
      generatedFiles += file
    }

    doc.enums.foreach {
      enum =>
        val file = new File(packageDir, enum.sid.toTitleCase.name + fileExtension)
        if (!dryRun) {
          writeFile(file, "", renderEnum(namespace, enum))
        }
        generatedFiles += file
    }

    doc.structs.foreach {
      struct =>
        val file = new File(packageDir, struct.sid.toTitleCase.name + fileExtension)

        if (!dryRun) {
          val rendered = struct match {
            case u: UnionDef => renderUnion(namespace, u)
            case s => renderStruct(Some(namespace), s)
          }

          writeFile(file, "", rendered)
        }
        generatedFiles += file
    }

    doc.services.foreach {
      service =>
        val interfaceFile = new File(packageDir, service.sid.toTitleCase.name + fileExtension)

        if (!dryRun) {
//          val dict = serviceDict(service, namespace, includes, serviceOptions)
//          writeFile(interfaceFile, templates.header, templates("service").generate(dict))
          writeFile(interfaceFile, "", renderService(namespace, service))
        }
        generatedFiles += interfaceFile
    }

    generatedFiles
  }
}
