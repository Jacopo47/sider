package com.sider.api

import com.sider.network.Resp3TcpClient
import com.sider.Type
import com.sider.SimpleString
import com.sider.BlobString
import com.sider.SimpleError
import com.sider.BlobError
import com.sider.Resp3Array

import com.sider.api.implementation.BasicStringCommands
import com.sider.api.implementation.BasicKeyCommands
case class ResponseNotMappedError() extends Throwable
case class KeyNotFound() extends Throwable

class RedisClient(
    host: Option[String] = Some("localhost"),
    port: Option[Int] = Some(6379)
) extends RedisApi {

  override val strings: StringCommands = new BasicStringCommands(tcpClient)
  override val keys: KeyCommands = new BasicKeyCommands(tcpClient)
  
  lazy val tcpClient: Resp3TcpClient = new Resp3TcpClient(host, port)
}