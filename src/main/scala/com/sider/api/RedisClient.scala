package com.sider.api

import com.sider.network.Resp3TcpClient
import com.sider.concurrency.IO
import com.sider.Type
import com.sider.SimpleString
import com.sider.BlobString

case class ResponseNotMappedError() extends Throwable
case class KeyNotFound() extends Throwable

class RedisClient(
    host: Option[String] = Some("localhost"),
    port: Option[Int] = Some(6379)
) extends RedisApi {

  override val strings: StringCommands = new BasicStringCommands(tcpClient)

  lazy val tcpClient: Resp3TcpClient = new Resp3TcpClient(host, port)

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

class BasicStringCommands(
    val tcp: Resp3TcpClient
) extends StringCommands {

  override def setRange(
      key: String,
      offset: Long,
      value: String
  ): Either[Throwable, Long] = ???

  override def mset(entries: Map[String, String]): Either[Throwable, String] =
    ???

  override def incr(key: String): Either[Throwable, String] = ???

  override def append(key: String, value: String): Either[Throwable, String] =
    ???

  override def incrByFloat(
      key: String,
      increment: Double
  ): Either[Throwable, String] = ???

  override def get(key: String): Either[Throwable, String] =
    tcp.sendAndWaitResponseSync(s"GET $key") flatMap {
      case v: SimpleString   => v.value
      case v: BlobString     => v.value
      case v: com.sider.Null => Left(KeyNotFound())
      case _                 => Left(ResponseNotMappedError())
    }

  override def decrBy(key: String, decrement: Long): Either[Throwable, Long] =
    ???

  override def strlen(key: String): Either[Throwable, Long] = ???

  override def decr(key: String): Either[Throwable, Long] = ???

  override def getEx(
      key: String,
      ex: Option[Long],
      px: Option[Long],
      exat: Option[Long],
      paxt: Option[Long],
      persist: Boolean
  ): Either[Throwable, String] = ???

  override def set(
      key: String,
      value: String,
      ex: Option[Long],
      px: Option[Long],
      exat: Option[Long],
      nx: Boolean,
      xx: Boolean,
      keepttl: Boolean,
      get: Boolean
  ): Either[Throwable, String] =
    tcp.sendAndWaitResponseSync(s"SET $key $value") flatMap {
      case v: SimpleString => v.value
      case _               => Left(ResponseNotMappedError())
    }

  override def mget(keys: String*): Either[Throwable, Seq[String]] = ???

  override def getDel(key: String): Either[Throwable, String] = ???

  override def incrBy(key: String, increment: Long): Either[Throwable, String] =
    ???

  override def msetNx(entries: Map[String, String]): Either[Throwable, Long] =
    ???

  override def getRange(
      key: String,
      start: Long,
      end: Long
  ): Either[Throwable, String] = ???

}
