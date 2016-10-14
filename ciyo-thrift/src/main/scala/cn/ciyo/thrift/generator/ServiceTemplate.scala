package cn.ciyo.thrift.generator

import cn.ciyo.thrift.ast._

trait ServiceTemplate { self: ScalaGenerator =>

  def renderService(namespace: Identifier, service: ServiceDef): String = {
    val ServiceName = genID(service.sid.toTitleCase)

    s"""
       |package ${genID(namespace)}
       |
       |import cn.ciyo.thrift.Thrift._
       |import org.apache.thrift.protocol._
       |import org.apache.thrift.TApplicationException
       |import scala.collection.immutable.{Map => immutable$$Map}
       |import scala.collection.mutable.{
       |  ArrayBuffer => mutable$$ArrayBuffer, Buffer => mutable$$Buffer,
       |  HashMap => mutable$$HashMap, HashSet => mutable$$HashSet}
       |import scala.collection.{Map, Set}
       |import scala.language.higherKinds
       |import scala.concurrent.{Future, Promise, ExecutionContext}
       |import scala.util.{Success, Failure}
       |
       |${service.docstring.getOrElse("")}
       |@javax.annotation.Generated(value = Array("cn.ciyo.thrift.Compiler"))
       |trait ${ServiceName}${ifShow(service.parent.isDefined)(s" extends ${getServiceParentType(service)}")} {
       |  ${loop(service.functions)(renderFunctionDef(_)).intent(1)}
       |}
       |
       |${service.docstring.getOrElse("")}
       |object ${ServiceName} { self =>
       |  ${loop(service.functions)(renderFunctionObj(service, _)).intent(1)}
       |
       |  class ${ServiceName}ThriftService(imp: ${ServiceName}) extends ${getServiceParentThriftServiceType(service)} {
       |    ${loop(service.functions){f => s"private val ${genID(f.funcName.toCamelCase)} = new ${genID(f.funcName.toTitleCase)}(imp.${genID(f.funcName.toCamelCase)})"}.intent(2)}
       |
       |    override def name = "${namespace.fullName}.${service.sid.originalName}"
       |    override def functions = Seq(${service.functions.map{f => genID(f.funcName.toCamelCase)}.mkString(", ")}) ++ super.functions
       |  }
       |
       |  class ${ServiceName}ThriftClient extends ${ServiceName} {
       |
       |  }
       |
       |  def apply(imp: ${ServiceName}) = new ${ServiceName}ThriftService(imp)
       |}
     """.stripMargin
  }

  def getServiceParentType(service: ServiceDef) = {
    service.parent.map { p =>
      genID(getServiceParentID(p))
    }.getOrElse("")
  }

  def getServiceParentThriftServiceType(service: ServiceDef) = {
    service.parent.map { p =>
      s"${genID(getServiceParentID(p))}.${genID(p.sid)}ThriftService(imp)"
    }.getOrElse("ThriftService")
  }

  def renderFunctionDef(f: Function) = {
    s"""
       |${f.docstring.getOrElse("")}
       |def ${genID(f.funcName.toCamelCase)}(${genFieldParams(f.args)}): Future[${genType(f.funcType)}]
     """.stripMargin
  }

  def renderFunctionObj(service: ServiceDef, f: Function) = {
    s"""
       |object ${genID(f.funcName.toTitleCase)} {
       |  ${renderStruct(None, getFunctionArgsStruct(f)).intent(1)}
       |  ${renderStruct(None, getFunctionResultStruct(f)).intent(1)}
       |
       |  val name = "${f.originalName}"
       |  val argsCodec = Args
       |  val resultCodec = Result
       |  val isOneway = ${if (f.funcType == OnewayVoid) "true" else "false"}
       |}
       |
       |class ${genID(f.funcName.toTitleCase)}(func: ${genFunctionType(f)}) extends ThriftFunction {
       |
       |  type Args = ${genID(f.funcName.toTitleCase)}.Args
       |  type SuccessType = ${genType(f.funcType)}
       |  type Result = ${genID(f.funcName.toTitleCase)}.Result
       |
       |  override def name = ${genID(f.funcName.toTitleCase)}.name
       |  override def argsCodec = ${genID(f.funcName.toTitleCase)}.Args
       |  override def resultCodec = ${genID(f.funcName.toTitleCase)}.Result
       |  override def isOneway = ${genID(f.funcName.toTitleCase)}.isOneway
       |
       |  override def invoke(args: ${genID(f.funcName.toTitleCase)}.Args)(implicit ec: ExecutionContext): Future[${genID(f.funcName.toTitleCase)}.Result] = {
       |    val p = Promise[Result]
       |    func(${f.args.map { f => "args." + genID(f.sid) }.mkString(", ")}).onComplete {
       |      case Success(r) =>
       |        ${ifShow(f.funcType == Void || f.funcType == OnewayVoid)("p.success(new Result())", "p.success(new Result(Some(r)))")}
       |      case Failure(e) => e match {
       |        ${loop(f.throws) { t => s"case e: ${genType(t.fieldType)} => p.success(new Result(${genID(t.sid)} = Some(e)))" }}
       |        case e => p.failure(e)
       |      }
       |    }
       |    p.future
       |  }
       |}
     """.stripMargin
  }

  private def getFunctionArgsStruct(f: Function): FunctionArgsDef = {
    FunctionArgsDef(SimpleID("Args"), f.funcName.name + "_args", f.args)
  }

  private def getFunctionResultStruct(f: Function): FunctionResultDef = {
    val throws = f.throws map {
      _.copy(requiredness = Requiredness.Optional)
    }
    val success = f.funcType match {
      case Void => None
      case OnewayVoid => None
      case fieldType: FieldType =>
        Some(Field(0, SimpleID("success"), "success", fieldType, None, Requiredness.Optional))
    }
    FunctionResultDef(SimpleID("Result"), f.funcName.name + "_result", success, throws)
  }

  private def genFunctionType(f: Function) = {
    s"(${f.args.map{a => genFieldType(a)}.mkString(", ")}) => Future[${genType(f.funcType)}]"
  }

}
