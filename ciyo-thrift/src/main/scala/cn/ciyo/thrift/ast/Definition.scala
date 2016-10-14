package cn.ciyo.thrift.ast

sealed abstract class Definition extends DefinitionNode {
  val sid: SimpleID
}

case class ConstDef(
  sid: SimpleID,
  fieldType: FieldType,
  value: RHS,
  docstring: Option[String]
) extends Definition

case class TypeDef(sid: SimpleID, fieldType: FieldType, annotations: Map[String, String] = Map.empty) extends Definition

case class EnumDef(
  sid: SimpleID,
  values: Seq[EnumFieldDef],
  docstring: Option[String],
  annotations: Map[String, String] = Map.empty
) extends Definition

case class EnumFieldDef(sid: SimpleID, value: Int, docstring: Option[String]) extends Definition
case class Senum(sid: SimpleID, values: Seq[String]) extends Definition

sealed abstract class StructLikeDef extends Definition {
  val originalName: String
  val fields: Seq[Field]
  val docstring: Option[String]
  val annotations: Map[String, String]
}

case class StructDef(
  sid: SimpleID,
  originalName: String,
  fields: Seq[Field],
  docstring: Option[String],
  annotations: Map[String, String] = Map.empty
) extends StructLikeDef

case class UnionDef(
  sid: SimpleID,
  originalName: String,
  fields: Seq[Field],
  docstring: Option[String],
  annotations: Map[String, String] = Map.empty
) extends StructLikeDef

case class FunctionArgsDef(
  sid: SimpleID,
  originalName: String,
  fields: Seq[Field]
) extends StructLikeDef {
  override val docstring: Option[String] = None
  override val annotations: Map[String, String] = Map.empty
}

case class FunctionResultDef(
  sid: SimpleID,
  originalName: String,
  success: Option[Field], // None for void methods
  exceptions: Seq[Field]
) extends StructLikeDef {
  override val fields = success.toList ++ exceptions
  override val docstring: Option[String] = None
  override val annotations: Map[String, String] = Map.empty
}

case class ExceptionDef(
  sid: SimpleID,
  originalName: String,
  fields: Seq[Field],
  docstring: Option[String],
  annotations: Map[String, String] = Map.empty
) extends StructLikeDef

case class ServiceDef(
  sid: SimpleID,
  parent: Option[ServiceParent],
  functions: Seq[Function],
  docstring: Option[String]
) extends Definition

/**
 * Identifier for the parent service.
  *
  * @param filename Set if the parent service is imported from another file
 */
case class ServiceParent(
  sid: SimpleID,
  filename: Option[SimpleID]
)
