package com.sider.api.implementation

import com.sider.network.Resp3TcpClient
import com.sider.Type
import com.sider.SimpleError
import com.sider.BlobError
import com.sider.api.ResponseNotMappedError
import com.sider.api.implementation.RedisApi.sendCommandWithGenericErrorHandler
import com.sider.BlobString
import com.sider.SimpleString
import com.sider.api.KeyNotFound
import com.sider.Resp3Array
import com.sider.Resp3Serialization
import com.sider.api.implementation.RedisApi.genericErrorHandler
import com.sider.api.options.ExpireOption
import com.sider.api.options.ListInsertPositionOption
import com.sider.api.entities.ScanResponse
import com.sider.api.RedisApiDefinition


case class ScanResponseNotAsExpected(val msg: String, val cause: Throwable = null) extends Throwable(msg, cause) {}
case class NotExistingOrEmptyList(val msg: String, val cause: Throwable = null) extends Throwable(msg, cause) {}

class RedisApi(
    val tcp: Resp3TcpClient
) extends RedisApiDefinition {

  given global: Resp3TcpClient = tcp

  /* Keys commands */
  
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



  /* Strings commands */

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


  /* List commands */
  
  override def lindex(key: String, index: Long): IO[Either[Throwable, String]] = ???
  override def linsert(key: String, position: ListInsertPositionOption, pivot: Long, element: String): IO[Either[Throwable, Long]] = ???
  override def llen(key: String): IO[Either[Throwable, Long]] = sendCommandWithGenericErrorHandler(Array("LLEN", key)) {
    case v: com.sider.Number => v.value 
  }
  override def lpush(key: String, elements: String*): IO[Either[Throwable, Long]] = sendCommandWithGenericErrorHandler("LPUSH" +: key +: elements.toArray) {
      case v: com.sider.Number => v.value
    }
  override def lpop(key: String, count: Option[Long] = None): IO[Either[Throwable, Seq[String]]] =
    var command = Array("LPOP", key)

    command =
      if count.isDefined then command :+ count.map(_.toString()).get else command

    sendCommandWithGenericErrorHandler(command) { 
      case v: com.sider.BlobString => v.value.map(Seq(_))
      case v: com.sider.Resp3Array => v.value.asInstanceOf[Either[Throwable, Seq[String]]]
      case v: com.sider.Null => Left(NotExistingOrEmptyList(s"Key $key does not exist or is an empty list"))
    }
}


object RedisApi {
  def genericErrorHandler[A]: PartialFunction[Type[?], Either[Throwable, A]] = {
    case e: SimpleError => e.value.flatMap(error => Left(Throwable(error)))
    case e: BlobError   => e.value.flatMap(error => Left(Throwable(error)))
    case _              => Left(ResponseNotMappedError())
  }

  def sendCommandWithGenericErrorHandler[A](
      command: Array[?]
  )(
      handler: PartialFunction[Type[?], Either[Throwable, A]]
  )(using tcp: Resp3TcpClient) =
    tcp
      .sendAndWaitResponseSync(command map (_.toString()): _*)
      .flatMap {
        handler orElse genericErrorHandler
      }
}