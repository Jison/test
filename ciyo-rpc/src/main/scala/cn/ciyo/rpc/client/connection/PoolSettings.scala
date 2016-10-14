package cn.ciyo.rpc.client.connection

import scala.concurrent.duration.Duration

case class PoolSettings(
	minConnections: Int,
	maxConnections: Int,
	maxOpenRequest: Int,
	pipeliningLimit: Int,
	idleTimeout: Duration
)
