package cn.ciyo.thrift.parser

import cn.ciyo.thrift.ast._
import org.scalatest.{WordSpec, _}

class ThriftParserSpec extends WordSpec with Matchers {
  "ThriftParser" should {
    val parser = new ThriftParser(NullImporter, true)

    val commentTestSources = Seq(
      "  300  ",
      "  // go away.\n 300",
      "  /*\n   * go away.\n   */\n 300",
      "# hello\n 300",
      "#\n300",
      "# \n300",
      "#    @\n300"
    )

    def verifyCommentParsing(source: String) =
      parser.parse(source, parser.rhs) should be(IntLiteral(300))

    "comments" in {
      commentTestSources.foreach(verifyCommentParsing)
    }

    "comments with Windows-style carriage return" in {
      commentTestSources.map(_.replace("\n","\r\n")).foreach(verifyCommentParsing)
    }

    "comments with parens" in {
      val source = """
# (
struct MyStruct {}
                   """
      parser.parse(source, parser.document) match {
        case Document(List(),List(StructDef(SimpleID("MyStruct", None), "MyStruct", List(),
        None, m))) if m.isEmpty =>
        case x => fail(s"Failed to match $x")
      }
    }

    "double-quoted strings" in {
      parser.parse(""" "hello!" """, parser.rhs) should be(StringLiteral("hello!"))
      parser.parse(""" "hello\nthere!" """, parser.rhs) should be(StringLiteral("""hello\nthere!"""))
      parser.parse(""" "hello\\nthere!" """, parser.rhs) should be(StringLiteral("""hello\\nthere!"""))
      parser.parse(""" "hello//there!" """, parser.rhs) should be(StringLiteral("""hello//there!"""))
      parser.parse(""" "hello'there!" """, parser.rhs) should be(StringLiteral("""hello'there!"""))
      parser.parse(""" "hello\'there!" """, parser.rhs) should be(StringLiteral("""hello\'there!"""))
      parser.parse(""" "hello\"there!" """, parser.rhs) should be(StringLiteral("""hello\"there!"""))
      parser.parse(""" "\"" """, parser.rhs) should be(StringLiteral("\\\""))
    }

    "single-quoted strings" in {
      parser.parse(""" 'hello!' """, parser.rhs) should be(StringLiteral("hello!"))
      parser.parse(""" 'hello\nthere!' """, parser.rhs) should be(StringLiteral("""hello\nthere!"""))
      parser.parse(""" 'hello\\nthere!' """, parser.rhs) should be(StringLiteral("""hello\\nthere!"""))
      parser.parse(""" 'hello//there!' """, parser.rhs) should be(StringLiteral("""hello//there!"""))
      parser.parse(""" 'hello"there!' """, parser.rhs) should be(StringLiteral("""hello"there!"""))
      parser.parse(""" 'hello\"there!' """, parser.rhs) should be(StringLiteral("""hello\"there!"""))
      parser.parse(""" 'hello\'there!' """, parser.rhs) should be(StringLiteral("""hello\'there!"""))
      parser.parse(""" '\'' """, parser.rhs) should be(StringLiteral("\\'"))
    }

    "constant" in {
      parser.parse("300.5", parser.rhs) should be(DoubleLiteral(300.5))
      parser.parse("cat", parser.rhs) should be(IdRHS(SimpleID("cat")))
      val list = parser.parse("[ 4, 5, ]", parser.rhs)
      list.isInstanceOf[ListRHS] should be(true)
      list.asInstanceOf[ListRHS].elems.toList should be(List(IntLiteral(4), IntLiteral(5)))
      parser.parse("{ 'name': 'Commie', 'home': 'San Francisco', }",
        parser.rhs) should be(MapRHS(Seq(StringLiteral("name") -> StringLiteral
      ("Commie"), StringLiteral("home") -> StringLiteral("San Francisco"))))
    }

    "base types" in {
      parser.parse("i16", parser.fieldType) should be(TI16)
      parser.parse("i32", parser.fieldType) should be(TI32)
      parser.parse("i64", parser.fieldType) should be(TI64)
      parser.parse("byte", parser.fieldType) should be(TByte)
      parser.parse("double", parser.fieldType) should be(TDouble)
      parser.parse("string", parser.fieldType) should be(TString)
      parser.parse("bool", parser.fieldType) should be(TBool)
      parser.parse("binary", parser.fieldType) should be(TBinary)
    }

    "compound types" in {
      parser.parse("list<i64>", parser.fieldType) should be(ListType(TI64, None))
      parser.parse("list<list<string>>", parser.fieldType) should be(ListType(ListType(TString,
        None), None))
      parser.parse("map<string, list<bool>>", parser.fieldType) should be(MapType(TString,
        ListType(TBool, None), None))
      parser.parse("set<Monster>", parser.fieldType) should be(SetType(ReferenceType(Identifier("Monster")),
        None))
      parser.parse("Monster", parser.fieldType) should be(ReferenceType(Identifier("Monster")))
    }

    "functions" in {
      parser.parse("/**doc!*/ void go()", parser.function) should be(
        Function(SimpleID("go"), "go", Void, Seq(), Seq(), Some("/**doc!*/")))
      parser.parse(
        "list<string> get_tables(optional i32 id, /**DOC*/3: required string name='cat') throws (1: Exception ex);",
        parser.function) should be(
        Function(SimpleID("get_tables"), "get_tables", ListType(TString, None), Seq(
          Field(-1, SimpleID("id"), "id", TI32, None, Requiredness.Optional),
          Field(3, SimpleID("name"), "name", TString, Some(StringLiteral("cat")), Requiredness.Required, docstring = Some("/**DOC*/"))
        ), Seq(Field(1, SimpleID("ex"), "ex", ReferenceType(Identifier("Exception")), None, Requiredness.Default)), None))
    }

    "const" in {
      parser.parse("/** COMMENT */ const string name = \"Columbo\"", parser.definition) should be(ConstDef(SimpleID("name"),
        TString, StringLiteral("Columbo"), Some("/** COMMENT */")))
    }

    "more than one docstring" in {
      val code = """
/** comment */
/** and another */
const string tyrion = "lannister"
                 """
      parser.parse(code, parser.definition) should be(ConstDef(SimpleID("tyrion"),
        TString, StringLiteral("lannister"), Some("/** comment */\n/** and another */")))
    }

    "comment before docstring" in {
      val code = """
#
/** docstring */
const string tyrion = "lannister"
                 """
      parser.parse(code, parser.definition) should be(ConstDef(SimpleID("tyrion"),
        TString, StringLiteral("lannister"), Some("/** docstring */")))
    }

    "typedef" in {
      parser.parse(
        """typedef list<i32> (information="important", more="better") Ladder""",
        parser.definition
      ) should be(
        TypeDef(
          SimpleID("Ladder"),
          ListType(TI32, None),
          Map("information" -> "important", "more" -> "better")
        ))
    }

    "enum" in {
      val code = """
        enum Direction {
          NORTH, SOUTH, EAST=90, WEST, UP, DOWN=5
        }
                 """
      parser.parse(code, parser.definition) should be(EnumDef(SimpleID("Direction"), Seq(
        EnumFieldDef(SimpleID("NORTH"), 0, None),
        EnumFieldDef(SimpleID("SOUTH"), 1, None),
        EnumFieldDef(SimpleID("EAST"), 90, None),
        EnumFieldDef(SimpleID("WEST"), 91, None),
        EnumFieldDef(SimpleID("UP"), 92, None),
        EnumFieldDef(SimpleID("DOWN"), 5, None)
      ), None))

      val withComment = """
/**
 * Docstring!
 */
enum Foo
{
  /** I am a doc. */
  // I am a comment.
  X = 1,
  // I am a comment.
  Y = 2
}"""
      parser.parse(withComment, parser.enum) should be(EnumDef(SimpleID("Foo"),
        Seq(
          EnumFieldDef(SimpleID("X"), 1, Some("/** I am a doc. */")),
          EnumFieldDef(SimpleID("Y"), 2, None)),
        Some("/**\n * Docstring!\n */")
      ))
    }


    "senum" in {
      // wtf is senum?!
      parser.parse("senum Cities { 'Milpitas', 'Mayfield' }", parser.definition) should be(
        Senum(SimpleID("Cities"), Seq("Milpitas", "Mayfield")))
    }

    "struct" in {
      val code = """
        /** docs up here */
        struct Point {
          1: double x
          /** comments*/
          2: double y
          3: Color color = BLUE
        } (
          annotation="supported",
          multiline="also supported",
        )
                 """
      parser.parse(code, parser.definition) should be(StructDef(SimpleID("Point"), "Point", Seq(
        Field(1, SimpleID("x"), "x", TDouble, None, Requiredness.Default),
        Field(2, SimpleID("y"), "y", TDouble, None, Requiredness.Default, docstring = Some("/** comments*/")),
        Field(3, SimpleID("color"), "color", ReferenceType(Identifier("Color")), Some(IdRHS(SimpleID("BLUE"))), Requiredness.Default)
      ), Some("/** docs up here */"), Map("annotation" -> "supported", "multiline" -> "also supported")))
    }

    "union" should {
      "basic" in {
        val code = """
          /** docs up here */
          union Aircraft {
            1: Airplane a
            /** comments*/
            2: Rotorcraft r
            3: Glider g
            4: LighterThanAir lta
          } (maxTypes="4")
                   """
        parser.parse(code, parser.definition) should be(UnionDef(SimpleID("Aircraft"), "Aircraft", Seq(
          Field(1, SimpleID("a"), "a", ReferenceType(Identifier("Airplane")), None, Requiredness.Default),
          Field(2, SimpleID("r"), "r", ReferenceType(Identifier("Rotorcraft")), None, Requiredness.Default, docstring = Some("/** comments*/")),
          Field(3, SimpleID("g"), "g", ReferenceType(Identifier("Glider")), None, Requiredness.Default),
          Field(4, SimpleID("lta"), "lta", ReferenceType(Identifier("LighterThanAir")), None, Requiredness.Default)
        ), Some("/** docs up here */"), Map("maxTypes" -> "4")))
      }

      "requiredness" in {
        intercept[UnionFieldRequiredException] {
          parser.parse("union Aircraft { 1: required Airplane a }", parser.definition)
        }
        intercept[UnionFieldOptionalException] {
          parser.parse("union Aircraft { 1: optional Airplane a }", parser.definition)
        }

        val laxParser = new ThriftParser(NullImporter, false)
        val code = """
          union Aircraft {
            1: required Airplane a
            2: optional Rotorcraft r
            3: Glider g
          }
                   """

        laxParser.parse(code, laxParser.definition) should be(UnionDef(SimpleID("Aircraft"), "Aircraft", Seq(
          Field(1, SimpleID("a"), "a", ReferenceType(Identifier("Airplane")), None, Requiredness.Default),
          Field(2, SimpleID("r"), "r", ReferenceType(Identifier("Rotorcraft")), None, Requiredness.Default),
          Field(3, SimpleID("g"), "g", ReferenceType(Identifier("Glider")), None, Requiredness.Default)
        ), None, Map.empty))
      }

      "invalid field name" in {
        intercept[UnionFieldInvalidNameException] {
          parser.parse("""
            union Fruit {
              1: Apple apple
              2: Banana banana
              3: UnknownFruit unknown_union_field
            }
                       """, parser.definition)
        }
      }
    }

    "exception" in {
      parser.parse("exception BadError { 1: string message }", parser.definition) should be(
        ExceptionDef(SimpleID("BadError"), "BadError",
          Seq(Field(1, SimpleID("message"), "message", TString, None, Requiredness.Default)), None))
      parser.parse("exception E { string message, string reason }", parser.definition) should be(
        ExceptionDef(SimpleID("E"), "E", Seq(
          Field(-1, SimpleID("message"), "message", TString, None, Requiredness.Default),
          Field(-2, SimpleID("reason"), "reason", TString, None, Requiredness.Default)
        ), None))
      parser.parse("exception NoParams { }", parser.definition) should be(
        ExceptionDef(SimpleID("NoParams"), "NoParams", Seq(), None))
      parser.parse("/** doc rivers */ exception wellDocumentedException { }", parser.definition) should be(
        ExceptionDef(SimpleID("wellDocumentedException"), "wellDocumentedException", Seq(), Some("/** doc rivers */")))

      val annotations = Map("persisted" -> "true")
      parser.parse("exception BadError { 1: string message } (persisted = \"true\")", parser.definition) should be(
        ExceptionDef(SimpleID("BadError"), "BadError",
          Seq(Field(1, SimpleID("message"), "message", TString, None, Requiredness.Default)), None, annotations))
    }

    "service" in {
      val code = """
        /** cold hard cache */
        service Cache {
          void put(1: string name, 2: binary value);
          binary get(1: string name) throws (1: NotFoundException ex);
        }
                 """
      parser.parse(code, parser.definition) should be(ServiceDef(SimpleID("Cache"), None, Seq(
        Function(SimpleID("put"), "put", Void, Seq(
          Field(1, SimpleID("name"), "name", TString, None, Requiredness.Default),
          Field(2, SimpleID("value"), "value", TBinary, None, Requiredness.Default)
        ), Seq(), None),
        Function(SimpleID("get"), "get", TBinary, Seq(
          Field(1, SimpleID("name"), "name", TString, None, Requiredness.Default)
        ), Seq(Field(1, SimpleID("ex"), "ex", ReferenceType(Identifier("NotFoundException")), None, Requiredness.Default)), None)
      ), Some("/** cold hard cache */")))

      parser.parse("service LeechCache extends Cache {}", parser.definition) should be(
        ServiceDef(
          SimpleID("LeechCache"),
          Some(ServiceParent(SimpleID("Cache"), None)),
          Seq(),
          None))
    }

    "document" in {
      val code = """
        namespace java com.example
        namespace * example

        /** what up doc */
        service NullService {
          /** DoC */
          void doNothing();
        }
                 """
      parser.parse(code, parser.document) should be(Document(
        Seq(Namespace("java", Identifier("com.example")), Namespace("*", Identifier("example"))),
        Seq(ServiceDef(SimpleID("NullService"), None, Seq(
          Function(SimpleID("doNothing"), "doNothing", Void, Seq(), Seq(), Some("/** DoC */"))
        ), Some("/** what up doc */")))
      ))
    }

    // reject syntax

    "reject negative field ids" in {
      val code =
        """
          struct Point {
            1: double x
            -2: double y
            3: Color color = BLUE
          }
        """
      intercept[NegativeFieldIdException] {
        parser.parse(code, parser.definition)
      }
    }

    "reject duplicate field ids" in {
      val code =
        """
          struct Point {
            1: double x
            2: double y
            2: Color color = BLUE
          }
        """
      intercept[DuplicateFieldIdException] {
        parser.parse(code, parser.definition)
      }
    }

    "reject duplicate enum values" in {
      intercept[RepeatingEnumValueException] {
        parser.parse("enum Bad { a=1, b, c=2 }", parser.definition)
      }

      val code = """
        enum Direction {
          NORTH, SOUTH, EAST=90, WEST=90, UP, DOWN=5
        }
                 """
      intercept[RepeatingEnumValueException] {
        parser.parse(code, parser.definition)
      }
    }

    "handle struct annotations" in {
      parser.parse(
        """typedef string (dbtype="fixedchar(4)", nullable="false") AirportCode""",
        parser.definition
      ) should be(
        TypeDef(
          SimpleID("AirportCode"),
          TString,
          Map("dbtype" -> "fixedchar(4)", "nullable" -> "false")
        ))

      val idTypeAnnotations = Map("autoincrement" -> "true")
      val idFieldAnnotations = Map("initialValue" -> "0")
      val codeTypeAnnotations = Map("dbtype" -> "varchar(255)")
      val nameFieldAnnotations = Map("postid" -> "varchar(255)")
      val structAnnotations = Map(
        "primary_key" -> "(id)",
        "index" -> "code_idx(code)",
        "sql_name" -> "airports"
      )
      val code =
        """
          struct Airport {
            1: optional i64 (autoincrement="true") id = 0(initialValue="0"),
            2: optional string(dbtype="varchar(255)") code,
            3: optional string name(postid="varchar(255)")
          } (primary_key="(id)",
             index="code_idx(code)",
             sql_name="airports",)
        """
      parser.parse(code, parser.definition) should be(
        StructDef(
          SimpleID("Airport"),
          "Airport",
          Seq(
            Field(1, SimpleID("id"), "id", TI64, Some(IntLiteral(0)), Requiredness.Default, idTypeAnnotations, idFieldAnnotations),
            Field(2, SimpleID("code"), "code", TString, None, Requiredness.Optional, codeTypeAnnotations, Map.empty),
            Field(3, SimpleID("name"), "name", TString, None, Requiredness.Optional, Map.empty, nameFieldAnnotations)
          ),
          None,
          structAnnotations
        ))
    }

    "handle illegal filenames" in {
      val illegalFilename = "illegal-name.thrift"

      intercept[InvalidThriftFilenameException] {
        getParserForFilenameTest(illegalFilename).parseFile(illegalFilename)
      }
    }

    "handle legal filenames" in {
      val illegalFilename = "legal_name.thrift"
      getParserForFilenameTest(illegalFilename).parseFile(illegalFilename)
    }

    "No thrift keywords as identifiers" in {
      val inputs = Seq(
        "struct MyStruct { 1: optional i64 struct }",
        "struct struct { 1: optional i64 count }",
        "enum list { alpha, beta }",
        "enum Stuff { alpha, beta, include }",
        "exception MyException { 1: string extends }",
        "service service { i32 getNum() }",
        "service MyService { i32 i32() }",
        "service MyService { i32 myMethod(1: bool optional) }",
        "typedef string binary"
      )

      inputs.foreach { code =>
        intercept[KeywordException] {
          parser.parse(code, parser.definition)
        }
      }
    }

    "boolean default values" in {
      var field = parser.parse("bool x = 0", parser.field)
      field.default should be (Some(BoolLiteral(false)))

      field = parser.parse("bool x = 1", parser.field)
      field.default should be (Some(BoolLiteral(true)))

      intercept[TypeMismatchException] {
        parser.parse("bool x = 2", parser.field)
      }

      field = parser.parse("bool x = false", parser.field)
      field.default should be (Some(BoolLiteral(false)))

      field = parser.parse("bool x = true", parser.field)
      field.default should be (Some(BoolLiteral(true)))

      field = parser.parse("bool x = False", parser.field)
      field.default should be (Some(BoolLiteral(false)))

      field = parser.parse("bool x = True", parser.field)
      field.default should be (Some(BoolLiteral(true)))

      intercept[TypeMismatchException] {
        parser.parse("bool x = WhatIsThis", parser.field)
      }

      parser.parse("const bool z = false", parser.const) should be (
        ConstDef(SimpleID("z", None), TBool, BoolLiteral(false), None))

      parser.parse("const bool z = True", parser.const) should be (
        ConstDef(SimpleID("z", None), TBool, BoolLiteral(true), None))

      intercept[TypeMismatchException] {
        parser.parse("const bool z = IDontEven", parser.const)
      }

      intercept[TypeMismatchException] {
        parser.parse("service theService { i32 getValue(1: bool arg = SomethingElse) }",
          parser.service)
      }

      parser.parse("struct asdf { bool x = false }", parser.struct) should be (
        StructDef(SimpleID("asdf", None), "asdf",
          List(Field(-1, SimpleID("x", None), "x", TBool, Some(BoolLiteral(false)),
            Requiredness.Default, Map(), Map())),
          None,Map()))

      parser.parse("struct asdf { bool x = 1 }", parser.struct) should be (
        StructDef(SimpleID("asdf", None), "asdf",
          List(Field(-1, SimpleID("x", None), "x", TBool, Some(BoolLiteral(true)),
            Requiredness.Default, Map(), Map())),
          None,Map()))

      intercept[TypeMismatchException] {
        parser.parse("struct S { 1: bool B = 15 }", parser.struct)
      }
    }
  }


  private def getParserForFilenameTest(thriftFilename: String): ThriftParser = {
    val importer = new Importer {
      override def apply(v1: String): scala.Option[FileContents] =
        scala.Some(FileContents(NullImporter, "", scala.Some(thriftFilename)))
      override private[thrift] def canonicalPaths: Seq[String] = Nil
      override def lastModified(filename: String): scala.Option[Long] = None
      override private[thrift] def getResolvedPath(filename: String): scala.Option[String] = Some(filename)
    }
    new ThriftParser(importer, true)
  }
}

