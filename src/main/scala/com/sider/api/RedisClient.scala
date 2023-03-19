package com.sider.api

import com.sider.network.Resp3TcpClient
import com.sider.concurrency.IO
import com.sider.Type
import com.sider.SimpleString
import com.sider.BlobString

class RedisClient(
    host: Option[String] = Some("localhost"),
    port: Option[Int] = Some(6379)
) {

  val tcpClient: Resp3TcpClient = new Resp3TcpClient(host, port)

  def set(key: String, value: String): IO[Either[Throwable, String]] =
    tcpClient.sendAndWaitResponse(s"SET $key $value") map {
      case Right(v: SimpleString) => v.value
      case _                      => Right("")
    }

  def get(key: String): IO[Either[Throwable, String]] = {
    tcpClient.sendAndWaitResponse(s"GET $key") map {
      case Right(v: SimpleString) => v.value
      case Right(v: BlobString)   => v.value
      case _                      => Right("")
    }
  }
}
