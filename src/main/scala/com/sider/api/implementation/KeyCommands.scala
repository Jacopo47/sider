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
import com.sider.api.entities.ScanResponse

case class ScanResponseNotAsExpected(val msg: String, val cause: Throwable = null) extends Throwable(msg, cause) {}

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

  def exists(key: String*): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler("EXISTS" +: key.toArray) {
      case v: com.sider.Number => v.value
    }

  def touch(key: String*): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler("TOUCH" +: key.toArray) {
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

  def persist(key: String): Either[Throwable, Long] = 
    sendCommandWithGenericErrorHandler(Array("PERSIST", key)) {
      case v: com.sider.Number => v.value
    }

  def pExpire(
      key: String,
      milliseconds: Long,
      option: Option[ExpireOption] = None
  ): Either[Throwable, Long] =
    var command = Array("PEXPIRE", key, milliseconds.toString())

    command =
      if option.isDefined then command :+ option.map(_.command).get else command

    sendCommandWithGenericErrorHandler(command) { 
      case v: com.sider.Number => v.value
    }

  def pExpireAt(
      key: String,
      unixTimeMilliseconds: Long,
      option: Option[ExpireOption] = None
  ): Either[Throwable, Long] =
    var command = Array("PEXPIREAT", key, unixTimeMilliseconds.toString())

    command =
      if option.isDefined then command :+ option.map(_.command).get else command

    sendCommandWithGenericErrorHandler(command) { 
      case v: com.sider.Number => v.value
    }

  def pExpireTime(key: String): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("PEXPIRETIME", key)) { 
      case v: com.sider.Number => v.value
    }

  def pTtl(key: String): Either[Throwable, Long] = 
    sendCommandWithGenericErrorHandler(Array("PTTL", key)) {
      case v: com.sider.Number => v.value
    }

  def ttl(key: String): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("TTL", key)) {
      case v: com.sider.Number => v.value
    }

  def keys(pattern: String): Either[Throwable, Seq[String]] =
    sendCommandWithGenericErrorHandler(Array("KEYS", pattern)) {
      case v: com.sider.Resp3Array =>
        v.value.asInstanceOf[Either[Throwable, Seq[String]]]
    }

  def objectEncoding(key: String): Either[Throwable, String] =
    sendCommandWithGenericErrorHandler(Array("OBJECT", "ENCODING", key)) {
      case v: com.sider.BlobString => v.value
    }

  def objectFreq(key: String): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("OBJECT", "FREQ", key)) {
      case v: com.sider.Number => v.value
    }
  def objectIdleTime(key: String): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("OBJECT", "IDLETIME", key)) {
      case v: com.sider.Number => v.value
    }
  def objectRefCount(key: String): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("OBJECT", "REFCOUNT", key)) {
      case v: com.sider.Number => v.value
    }

  def randomKey(): Either[Throwable, String] =
    sendCommandWithGenericErrorHandler(Array("RANDOMKEY")) {
      case v: com.sider.BlobString => v.value
    }

  def rename(src: String, dest: String): Either[Throwable, String] =
    sendCommandWithGenericErrorHandler(Array("RENAME", src, dest)) {
      case v: com.sider.SimpleString => v.value
    }

  def renameNx(src: String, dest: String): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler(Array("RENAMENX", src, dest)) {
      case v: com.sider.Number => v.value
    }

  def scan(
    cursor: String, 
    pattern: Option[String] = None,
    count: Option[Long] = None,
    _type: Option[String] = None
  ): Either[Throwable, ScanResponse] =
    var command = Array("SCAN", cursor)

    command = if (pattern.isDefined) {
      command :++ Array("MATCH", pattern.get)
    } else {
      command
    }

    command = if (count.isDefined) {
      command :++ Array("COUNT", count.map(_.toString()).get)
    } else {
      command
    }

    command = if (_type.isDefined) {
      command :++ Array("TYPE", _type.get)
    } else {
      command
    }

    sendCommandWithGenericErrorHandler(command) {
      case v: com.sider.Resp3Array => v.value
    }
    .filterOrElse(_.size == 2, ScanResponseNotAsExpected(s"Expected 2 elements from the SCAN command response"))
    .flatMap(res => {
      try {
        val cursor: String = res(0).asInstanceOf[String]
        val elements: Seq[String] = res(1).asInstanceOf[Seq[String]]

        Right(ScanResponse(cursor, elements))
      } catch {
        case e: ClassCastException => Left(ScanResponseNotAsExpected("Unexpected type", e))
      }
    })


  def unlink(key: String*): Either[Throwable, Long] =
    sendCommandWithGenericErrorHandler("UNLINK" +: key.toArray) {
      case v: com.sider.Number => v.value
    }

  def type_(key: String): Either[Throwable, String] =
    sendCommandWithGenericErrorHandler(Array("TYPE", key)) {
      case v: com.sider.SimpleString => v.value
    }
}
