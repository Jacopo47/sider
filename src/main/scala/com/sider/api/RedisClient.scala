package com.sider.api

import com.sider.network.Resp3TcpClient
import com.sider.concurrency.IO
import com.sider.Type
import com.sider.SimpleString
import com.sider.BlobString
import com.sider.SimpleError

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

// TODO Find a way a clever to map SimpleError / BlobError to Left(Throwable())

class BasicStringCommands(
    val tcp: Resp3TcpClient
) extends StringCommands {

  override def append(key: String, value: String): Either[Throwable, String] =
    ???

  override def setRange(
      key: String,
      offset: Long,
      value: String
  ): Either[Throwable, Long] = ???

  override def mset(entries: Map[String, String]): Either[Throwable, String] =
    ???

  override def incr(key: String): Either[Throwable, Long] =
    tcp.sendAndWaitResponseSync(s"INCR $key") flatMap {
      case v: com.sider.Number => v.value
      case _                   => Left(ResponseNotMappedError())
    }

  override def incrBy(key: String, increment: Long): Either[Throwable, Long] =
    tcp.sendAndWaitResponseSync(s"INCRBY $key $increment") flatMap {
      case v: com.sider.Number => v.value
      case _                   => Left(ResponseNotMappedError())
    }

  override def incrByFloat(
      key: String,
      increment: Double
  ): Either[Throwable, Double] =
    tcp.sendAndWaitResponseSync(s"INCRBYFLOAT $key $increment") flatMap {
      case v: BlobString => v.value map { _.toDouble }
      case _             => Left(ResponseNotMappedError())
    }

  override def get(key: String): Either[Throwable, String] =
    tcp.sendAndWaitResponseSync(s"GET $key") flatMap {
      case v: SimpleString   => v.value
      case v: BlobString     => v.value
      case v: com.sider.Null => Left(KeyNotFound())
      case _                 => Left(ResponseNotMappedError())
    }

  override def strlen(key: String): Either[Throwable, Long] = ???

  override def decr(key: String): Either[Throwable, Long] =     
    tcp.sendAndWaitResponseSync(s"DECR $key") flatMap {
      case v: com.sider.Number => v.value
      case _                   => Left(ResponseNotMappedError())
    }

  override def decrBy(key: String, decrement: Long): Either[Throwable, Long] =
    tcp.sendAndWaitResponseSync(s"DECRBY $key $decrement") flatMap {
      case v: com.sider.Number => v.value
      case _                   => Left(ResponseNotMappedError())
    }

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
      pxat: Option[Long],
      nx: Boolean,
      xx: Boolean,
      keepttl: Boolean,
      get: Boolean
  ): Either[Throwable, String] =
    val command: Seq[String] = Seq(
      "SET",
      key,
      value,
      if nx then "NX" else null,
      if xx then "XX" else null,
      if get then "GET" else null,
      ex.map(_.toString()).getOrElse(null),
      px.map(_.toString()).getOrElse(null),
      exat.map(_.toString()).getOrElse(null),
      pxat.map(_.toString()).getOrElse(null),
      if keepttl then "KEEPTTL" else null
    )
      .filter(_ != null)

    tcp.sendAndWaitResponseSync(command: _*) flatMap {
      case v: SimpleString => v.value
      case _               => Left(ResponseNotMappedError())
    }

  override def mget(keys: String*): Either[Throwable, Seq[String]] = ???

  override def getDel(key: String): Either[Throwable, String] = ???

  override def msetNx(entries: Map[String, String]): Either[Throwable, Long] =
    ???

  override def getRange(
      key: String,
      start: Long,
      end: Long
  ): Either[Throwable, String] = ???

}
