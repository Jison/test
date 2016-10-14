package cn.ciyo.thrift.ast

case class Document(headers: Seq[Header], defs: Seq[Definition]) extends DocumentNode {
  def namespace(language: String): Option[Identifier] = {
    (headers collect {
      // first try to find language specific namespace scope
      case Namespace(l, x) if l == language => x
    }).headOption orElse(headers collect {
      // then see if universal namespace scope is defined
      case Namespace(l, x) if l == "*" => x
    }).headOption
  }

  def mapNamespaces(namespaceMap: Map[String,String]): Document = {
    copy(
      headers = headers map {
        case header @ Namespace(_, ns) =>
          namespaceMap.get(ns.fullName) map {
            newNs => header.copy(id = Identifier(newNs))
          } getOrElse(header)

        case include @ Include(_, doc) =>
          include.copy(document = doc.mapNamespaces(namespaceMap))

        case header => header
      }
    )
  }

  def consts: Seq[ConstDef] = defs.collect { case c: ConstDef => c }
  def enums: Seq[EnumDef] = defs.collect { case e: EnumDef => e }
  def structs: Seq[StructLikeDef] = defs.collect { case s: StructLikeDef => s }
  def services: Seq[ServiceDef] = defs.collect { case s: ServiceDef => s }
}