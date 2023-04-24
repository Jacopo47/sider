package com.sider.api.implementation

import com.sider.api.KeyCommands
import com.sider.network.Resp3TcpClient
import com.sider.BlobString
import java.nio.charset.StandardCharsets
import com.sider.Resp3Serialization
import com.sider.Identifiers.SimpleString
import com.sider.Type
import com.sider.SimpleString
import com.sider.api.options.ExpireOption

class BasicKeyCommands(
    val tcp: Resp3TcpClient
) extends KeyCommands {

  given global: Resp3TcpClient = tcp

  override def copy(
      source: String,
      dest: String,
      db: Option[String] = None,
      replace: Boolean = false
  ): Either[Throwable, Long] =
    var commands = Array("COPY", source, dest)

    commands = if db.isDefined then commands :+ "DB" :+ db.get else commands
    commands = if replace then commands :+ "REPLACE" else commands

    sendCommandWithGenericErrorHandler(commands) { case v: com.sider.Number =>
      v.value
    }

  override def del(key: String*): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler("DEL" +: key.toArray) {
      case v: com.sider.Number => v.value
    }

  override def dump(key: String): Either[Throwable, Array[Byte]] =
    sendCommandWithGenericErrorHandler(Array("DUMP", key)) {
      case v: BlobString =>
        v.bytes
          .map(Resp3Serialization.skip(_))
          .map(_.dropRight(2))
          .map(_.toArray)
    }

  override def restore(
      key: String,
      serializedValue: Array[Byte],
      ttl: Option[Long] = Some(0),
      replace: Boolean = false,
      absTtl: Boolean = false,
      idleTime: Option[Long] = None,
      freq: Option[Long] = None
  ): Either[Throwable, String] =
    var commands = Array(
      "RESTORE".getBytes(),
      key.getBytes(),
      ttl.map(_.toString()).getOrElse("0").getBytes(),
      serializedValue
    )

    commands = if replace then commands :+ "REPLACE".getBytes() else commands
    commands = if absTtl then commands :+ "ABSTTL".getBytes() else commands
    commands =
      if idleTime.isDefined then
        commands :++ Array("IDLETIME", idleTime.map(_.toString()).get)
          .map(_.getBytes())
      else commands
    commands =
      if freq.isDefined then
        commands :++ Array("FREQ", freq.get.toString()).map(_.getBytes())
      else commands

    val handler: PartialFunction[Type[?], Either[Throwable, String]] = {
      case v: SimpleString => v.value
    }

    tcp
      .sendAndWaitResponse(Resp3Serialization.toCommandFromBytes(commands: _*))
      .get
      .flatMap {
        handler orElse genericErrorHandler
      }

  override def exists(key: String*): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler("EXISTS" +: key.toArray) {
      case v: com.sider.Number => v.value
    }

  def expire(
      key: String,
      seconds: Long,
      option: Option[ExpireOption] = None
  ): Either[Throwable, Long] =
    var command = Array("EXPIRE", key, seconds.toString())

    command =
      if option.isDefined then command :+ option.map(_.command).get else command

    sendCommandWithGenericErrorHandler(command) { case v: com.sider.Number =>
      v.value
    }

  def expireAt(
      key: String,
      unixTimeSecond: Long,
      option: Option[ExpireOption] = None
  ): Either[Throwable, Long] =
    var command = Array("EXPIREAT", key, unixTimeSecond.toString())

    command =
      if option.isDefined then command :+ option.map(_.command).get else command

    sendCommandWithGenericErrorHandler(command) { case v: com.sider.Number =>
      v.value
    }

  def expireTime(key: String): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("EXPIRETIME", key)) {
      case v: com.sider.Number => v.value
    }

  def ttl(key: String): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("TTL", key)) {
      case v: com.sider.Number => v.value
    }

}
