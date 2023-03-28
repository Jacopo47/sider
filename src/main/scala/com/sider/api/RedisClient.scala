package com.sider.api

import com.sider.network.Resp3TcpClient
import com.sider.Type
import com.sider.SimpleString
import com.sider.BlobString
import com.sider.SimpleError
import com.sider.BlobError
import com.sider.Resp3Array

case class ResponseNotMappedError() extends Throwable
case class KeyNotFound() extends Throwable

class RedisClient(
    host: Option[String] = Some("localhost"),
    port: Option[Int] = Some(6379)
) extends RedisApi {

  override val strings: StringCommands = new BasicStringCommands(tcpClient)

  lazy val tcpClient: Resp3TcpClient = new Resp3TcpClient(host, port)
}

class BasicStringCommands(
    val tcp: Resp3TcpClient
) extends StringCommands {

  given global: Resp3TcpClient = tcp

  override def append(key: String, value: String): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("APPEND", key, value)) {
      case n: com.sider.Number => n.value
    }

  override def incr(key: String): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("INCR", key)) {
      case v: com.sider.Number => v.value
    }

  override def incrBy(key: String, increment: Long): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("INCRBY", key, increment)) {
      case v: com.sider.Number => v.value
    }

  override def incrByFloat(
      key: String,
      increment: Double
  ): Either[Throwable, Double] =
    sendCommandWithGenericErrorHandler(Array("INCRBYFLOAT", key, increment)) {
      case v: BlobString => v.value map { _.toDouble }
    }

  override def get(key: String): Either[Throwable, String] =
    sendCommandWithGenericErrorHandler(Array("GET", key)) {
      case v: SimpleString   => v.value
      case v: BlobString     => v.value
      case v: com.sider.Null => Left(KeyNotFound())

    }

  override def strlen(key: String): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("STRLEN", key)) {
      case v: com.sider.Number => v.value
    }

  override def decr(key: String): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("DECR", key)) {
      case v: com.sider.Number => v.value

    }

  override def decrBy(key: String, decrement: Long): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("DECRBY", key, decrement)) {
      case v: com.sider.Number => v.value
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
    val command = Array(
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

    sendCommandWithGenericErrorHandler(command) { 
      case v: SimpleString => v.value
    }

  override def mget(keys: String*): Either[Throwable, Seq[Any]] = sendCommandWithGenericErrorHandler("MGET" +: keys.toArray) { 
      case v: Resp3Array => v.value
    }

  override def getDel(key: String): Either[Throwable, String] =
    sendCommandWithGenericErrorHandler(Array("GETDEL", key)) {
      case v: BlobString     => v.value
      case v: com.sider.Null => Left(KeyNotFound())
    }

  override def mset(entries: Map[String, String]): Either[Throwable, String] =
    val command = entries
      .toArray
      .flatMap(e => Array(e._1, e._2))
    sendCommandWithGenericErrorHandler("MSET" +: command) {
      case v: com.sider.SimpleString => v.value
    }

  override def msetNx(entries: Map[String, String]): Either[Throwable, Long] =
    val command = entries
      .toArray
      .flatMap(e => Array(e._1, e._2))
    sendCommandWithGenericErrorHandler("MSETNX" +: command) {
      case v: com.sider.Number => v.value
    }

  override def setRange(
      key: String,
      offset: Long,
      value: String
  ): Either[Throwable, Long] = sendCommandWithGenericErrorHandler(Array("SETRANGE", key, offset, value)) {
    case v: com.sider.Number => v.value
  }
    

  override def getRange(
      key: String,
      start: Long,
      end: Long
  ): Either[Throwable, String] = sendCommandWithGenericErrorHandler(Array("GETRANGE", key, start, end)) {
    case v: BlobString => v.value
  }

}
