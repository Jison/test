package cn.ciyo.rpc.model

trait ServiceId

class ServiceIdImp(id: String) extends ServiceId {
	override def toString: String = id
}

object ServiceId {
	def apply(packageName: String, serviceName: String, methodName: String) =
		new ServiceIdImp(packageName + "." + serviceName + ":" + methodName)

	def apply(serviceNameWithPackage: String, methodName: String) =
		new ServiceIdImp(serviceNameWithPackage + ":" + methodName)
}
