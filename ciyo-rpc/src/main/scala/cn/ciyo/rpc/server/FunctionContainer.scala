package cn.ciyo.rpc.server

import org.slf4j.LoggerFactory


class FunctionContainer {

  val logger = LoggerFactory.getLogger(classOf[FunctionContainer])

//  private var functionMap: Map[String, ThriftFunction] = Map()
//
//  def add(service: ThriftService): Unit = {
//    functionMap ++= service.functions.map{f =>
//      (service.name + ":" + f.name, f)
//    }
//
//    logger.debug("functions: {}", functionMap)
//  }
//
//  def getFunction(name: String): Option[ThriftFunction] = {
//    functionMap.get(name)
//  }

}
