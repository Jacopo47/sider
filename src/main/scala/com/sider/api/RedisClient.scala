package com.sider.api

import com.sider.network.Resp3TcpClient
import com.sider.Type
import com.sider.SimpleString
import com.sider.BlobString
import com.sider.SimpleError
import com.sider.BlobError
import com.sider.Resp3Array
import com.sider.api.implementation.RedisApi


case class ResponseNotMappedError() extends Throwable
case class KeyNotFound() extends Throwable

class RedisClient(
    host: Option[String] = Some("localhost"),
    port: Option[Int] = Some(6379)
) {
  lazy val tcpClient: Resp3TcpClient = new Resp3TcpClient(host, port)

  lazy val api: RedisApiDefinition = new RedisApi(tcpClient)
}