package cn.ciyo.thrift.generator

import cn.ciyo.thrift.ast.{ConstDef, Identifier}

trait ConstsTemplate { self: ScalaGenerator =>

  def renderConsts(namespace: Identifier, consts: Seq[ConstDef]): String = {
    s"""
      |package ${genID(namespace)}
      |
      |@javax.annotation.Generated(value = Array("cn.ciyo.thrift.Compiler"))
      |object Constants {
      |  ${loop(consts)(renderConst).intent(1)}
      |}
    """.stripMargin
  }

  private def renderConst(const: ConstDef): String = {
    s"""
       |${const.docstring.getOrElse("")}
       |val ${genID(const.sid)}: ${genType(const.fieldType)} = ${genConstant(const.value, Some(const.fieldType))}
     """.stripMargin
  }
}