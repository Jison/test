package cn.ciyo.thrift.generator

import cn.ciyo.thrift.ast.{EnumDef, EnumFieldDef, Identifier}

trait EnumTemplate { self: ScalaGenerator =>

  def renderEnum(namespace: Identifier, enum: EnumDef): String = {
    val EnumName = genID(enum.sid.toTitleCase)
    s"""
       |package ${genID(namespace)}
       |
       |import cn.ciyo.thrift.Thrift.ThriftEnum
       |
       |${enum.docstring.getOrElse("")}
       |@javax.annotation.Generated(value = Array("cn.ciyo.thrift.Compiler"))
       |case object ${EnumName} {
       |  ${loop(enum.values)(renderValue(EnumName)).intent(1)}
       |
       |  case class EnumUnknown${EnumName}(value: Int) extends ${EnumName} {
       |    val name = "EnumUnknown${EnumName}" + value
       |  }
       |
       |  /**
       |   * Find the enum by its integer value, as defined in the Thrift IDL.
       |   * @throws NoSuchElementException if the value is not found.
       |   */
       |  def apply(value: Int): ${EnumName} =
       |    value match {
       |      ${loop(enum.values){v => s"case ${v.value.toString} => ${EnumName}.${genID(v.sid)}"}.intent(3)}
       |      case _ => throw new NoSuchElementException(value.toString)
       |    }
       |
       |  /**
       |   * Find the enum by its integer value, as defined in the Thrift IDL.
       |   * returns an EnumUnknown${EnumName}(value) if the value is not found.
       |   * In particular this allows ignoring new values added to an enum
       |   * in the IDL on the producer side when the consumer was not updated.
       |   */
       |  def getOrUnknown(value: Int): ${EnumName} = get(value) match {
       |    case Some(e) => e
       |    case None => EnumUnknown${EnumName}(value)
       |  }
       |
       |  /**
       |   * Find the enum by its integer value, as defined in the Thrift IDL.
       |   * Returns None if the value is not found
       |   */
       |  def get(value: Int): Option[${EnumName}] = value match {
       |    ${loop(enum.values){v => s"case ${v.value.toString} => _Some${genID(v.sid)}"}.intent(2)}
       |    case _ => None
       |  }
       |
       |  def valueOf(name: String): Option[${EnumName}] = name.toLowerCase match {
       |    ${loop(enum.values){v => s"""case "${v.sid.fullName.toLowerCase}" => _Some${genID(v.sid)}"""}.intent(2)}
       |    case _ => None
       |  }
       |
       |  lazy val list: List[${EnumName}] = List[${EnumName}](
       |    ${enum.values.map{v => s"${EnumName}.${genID(v.sid)}"}.mkString(", ")}
       |  )
       |}
       |
       |${enum.docstring.getOrElse("")}
       |@javax.annotation.Generated(value = Array("cn.ciyo.thrift.Compiler"))
       |sealed trait ${EnumName} extends ThriftEnum with Serializable
     """.stripMargin
  }

  private def renderValue(EnumName: String)(value: EnumFieldDef): String = {
    s"""
       |${value.docstring.getOrElse("")}
       |case object ${genID(value.sid)} extends ${EnumName} {
       |  val value = ${value.value.toString}
       |  val name = "${genID(value.sid)}"
       |  val originalName = "${value.sid.originalName}"
       |}
       |
       |private[this] val _Some${genID(value.sid)} = Some(${EnumName}.${genID(value.sid)})
     """.stripMargin
  }

}
