package com.sider.api.implementation

import com.sider.BlobString
import com.sider.network.Resp3TcpClient
import com.sider.SimpleString
import com.sider.Resp3Array
import com.sider.api.StringCommands
import com.sider.api.KeyNotFound

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
      pxat: Option[Long],
      persist: Boolean
  ): Either[Throwable, String] =
    val command = Array(
      Array("GETEX"),
      Array(key),
      ex.map(_.toString()).map(Array("EX", _)).getOrElse(null),
      px.map(_.toString()).map(Array("PX", _)).getOrElse(null),
      exat.map(_.toString()).map(Array("EXAT", _)).getOrElse(null),
      pxat.map(_.toString()).map(Array("PXAT", _)).getOrElse(null),
      if persist then Array("PERSIST") else null
    )
      .filter(_ != null)
      .flatten

    sendCommandWithGenericErrorHandler(command) { case v: BlobString =>
      v.value
    }

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
      Array("SET"),
      Array(key),
      Array(value),
      if nx then Array("NX") else null,
      if xx then Array("XX") else null,
      if get then Array("GET") else null,
      ex.map(_.toString()).map(Array("EX", _)).getOrElse(null),
      px.map(_.toString()).map(Array("PX", _)).getOrElse(null),
      exat.map(_.toString()).map(Array("EXAT", _)).getOrElse(null),
      pxat.map(_.toString()).map(Array("PXAT", _)).getOrElse(null),
      if keepttl then Array("KEEPTTL") else null
    )
      .filter(_ != null)
      .flatten

    sendCommandWithGenericErrorHandler(command) {
      case v: SimpleString => v.value
      case v: BlobString   => v.value
    }

  override def mget(keys: String*): Either[Throwable, Seq[Any]] =
    sendCommandWithGenericErrorHandler("MGET" +: keys.toArray) {
      case v: Resp3Array => v.value
    }

  override def getDel(key: String): Either[Throwable, String] =
    sendCommandWithGenericErrorHandler(Array("GETDEL", key)) {
      case v: BlobString     => v.value
      case v: com.sider.Null => Left(KeyNotFound())
    }

  override def mset(entries: Map[String, String]): Either[Throwable, String] =
    val command = entries.toArray
      .flatMap(e => Array(e._1, e._2))
    sendCommandWithGenericErrorHandler("MSET" +: command) {
      case v: com.sider.SimpleString => v.value
    }

  override def msetNx(entries: Map[String, String]): Either[Throwable, Long] =
    val command = entries.toArray
      .flatMap(e => Array(e._1, e._2))
    sendCommandWithGenericErrorHandler("MSETNX" +: command) {
      case v: com.sider.Number => v.value
    }

  override def setRange(
      key: String,
      offset: Long,
      value: String
  ): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("SETRANGE", key, offset, value)) {
      case v: com.sider.Number => v.value
    }

  override def getRange(
      key: String,
      start: Long,
      end: Long
  ): Either[Throwable, String] =
    sendCommandWithGenericErrorHandler(Array("GETRANGE", key, start, end)) {
      case v: BlobString => v.value
    }

}
