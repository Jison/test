package cn.ciyo.thrift

import java.io.{File, FileWriter}
import java.util.Properties

import cn.ciyo.thrift.ast.Document
import cn.ciyo.thrift.generator.ScalaGenerator
import cn.ciyo.thrift.parser.{FileParseException, Importer, ThriftParser, TypeResolver}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scopt.OptionParser

object Compiler {
  object Defaults {
    var language: String = "scala"
    var defaultNamespace: String = "thrift"
  }

  def compile(args: Array[String]) = {
    val compiler = new Compiler()
    if (!parseOptions(compiler, args)) {
      System.exit(1)
    }
    compiler.run()
  }

  def parseOptions(compiler: Compiler, args: Seq[String]): Boolean = {
    val buildProperties = new Properties
    scala.Option(getClass.getResource("build.properties")) foreach { resource =>
      buildProperties.load(resource.openStream)
    }

    val parser = new OptionParser[Compiler]("scrooge") {
      help("help").text("show this help screen")

      override def showUsageOnError: Boolean = true

      opt[Unit]('V', "version").action { (_, c) =>
        println("scrooge " + buildProperties.getProperty("version", "0.0"))
        println("    build " + buildProperties.getProperty("build_name", "unknown"))
        println("    git revision " + buildProperties.getProperty("build_revision", "unknown"))
        System.exit(0)
        c
      }.text("print version and quit")

      opt[Unit]('v', "verbose").action { (_, c) =>
        c.verbose = true
        c
      }.text("log verbose messages about progress")

      opt[String]('d', "dest").valueName("<path>").action { (d, c) =>
        c.destFolder = d
        c
      }.text("write generated code to a folder (default: %s)".format(compiler.defaultDestFolder))

      opt[String]('i', "include-path").unbounded().valueName("<path>").action { (path, c) =>
        c.includePaths ++= path.split(File.pathSeparator)
        c
      }.text("path(s) to search for included thrift files (may be used multiple times)")

      opt[String]('n', "namespace-map").unbounded().valueName("<oldname>=<newname>").action { (mapping, c) =>
        mapping.split("=") match {
          case Array(from, to) =>
            c.namespaceMappings(from) = to
            c
        }
      }.text("map old namespace to new (may be used multiple times)")

      opt[String]("default-java-namespace").unbounded().valueName("<name>").action { (name, c) =>
        c.defaultNamespace = name
        c
      }.text("Use <name> as default namespace if the thrift file doesn't define its own namespace. " +
        "If this option is not specified either, then use \"thrift\" as default namespace")

      opt[Unit]("disable-strict").action { (_, c) =>
        c.strict = false
        c
      }.text("issue warnings on non-severe parse errors instead of aborting")

      opt[String]("gen-file-map").valueName("<path>").action { (path, c) =>
        c.fileMapPath = Some(path)
        c
      }.text("generate map.txt in the destination folder to specify the mapping from input thrift files to output Scala/Java files")

      opt[Unit]("dry-run").action { (_, c) =>
        c.dryRun = true
        c
      }.text("parses and validates source thrift files, reporting any errors, but" +
        " does not emit any generated source code.  can be used with " +
        "--gen-file-mapping to get the file mapping")

      opt[String]("experiment-flag").valueName("<flag>").unbounded().action { (flag, c) =>
        c.experimentFlags += flag
        c
      }.text("[EXPERIMENTAL] DO NOT USE FOR PRODUCTION. This is meant only for enabling/disabling features for benchmarking")

      arg[String]("<files...>").unbounded().action { (files, c) =>
        c.thriftFiles += files
        c
      }.text("thrift files to compile")
    }
    val parsed = parser.parse(args, compiler)
    parsed.isDefined
  }

  def isUnchanged(file: File, sourceLastModified: Long): Boolean = {
    file.exists && file.lastModified >= sourceLastModified
  }
}

class Compiler {
  val defaultDestFolder = "."
  var destFolder: String = defaultDestFolder
  val includePaths = new mutable.ListBuffer[String]
  val thriftFiles = new mutable.ListBuffer[String]
  val namespaceMappings = new mutable.HashMap[String, String]
  var verbose = false
  var strict = true
  var experimentFlags = new mutable.ListBuffer[String]
  var fileMapPath: scala.Option[String] = None
  var fileMapWriter: scala.Option[FileWriter] = None
  var dryRun: Boolean = false
  var language: String = Compiler.Defaults.language
  var defaultNamespace: String = Compiler.Defaults.defaultNamespace

  def run() {
    // if --gen-file-map is specified, prepare the map file.
    fileMapWriter = fileMapPath.map { path =>
      val file = new File(path)
      val dir = file.getParentFile
      if (dir != null && !dir.exists()) {
        dir.mkdirs()
      }
      if (verbose) {
        println("+ Writing file mapping to %s".format(path))
      }
      new FileWriter(file)
    }

    val importer = Importer(new File(".")) +: Importer(includePaths)

    val isJava = language.equals("java")
    val documentCache = new TrieMap[String, Document]

    // compile
    for (inputFile <- thriftFiles) {
      try {
        val parser = new ThriftParser(
          importer,
          strict,
          defaultOptional = isJava,
          skipIncludes = false,
          documentCache
        )
        val doc = parser.parseFile(inputFile).mapNamespaces(namespaceMappings.toMap)

        if (verbose) println("+ Compiling %s".format(inputFile))
        val resolvedDoc = TypeResolver()(doc)

        val generator = new ScalaGenerator(resolvedDoc, "thrift", experimentFlags)

        val generatedFiles = generator(
          new File(destFolder),
          dryRun
        ).map {
          _.getPath
        }
        if (verbose) {
          println("+ Generated %s".format(generatedFiles.mkString(", ")))
        }
        fileMapWriter.foreach { w =>
          generatedFiles.foreach { path =>
            w.write(inputFile + " -> " + path + "\n")
          }
        }
      } catch {
        case e: Throwable => throw new FileParseException(inputFile, e)
      }
    }

    // flush and close the map file
    fileMapWriter.foreach { _.close() }
  }
}
