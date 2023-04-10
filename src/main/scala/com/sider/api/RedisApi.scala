package com.sider.api

import scala.util.Try
import com.sider.Type
import com.sider.SimpleError
import com.sider.BlobError
import com.sider.network.Resp3TcpClient

/**
 * These API aim to be transparent as much as possible. So, until something
 * different is explicit specified by the doc all these method are just
 * utilities in order to call "plain" Redis command. Refer to Redis' commands
 * documentation for details about command's behavior.
 */
trait RedisApi {
  val strings: StringCommands
  val keys: KeyCommands
}

sealed trait RedisCommandsByType {
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

trait KeyCommands extends RedisCommandsByType {
  def copy(source: String, dest: String, db: Option[String] = None, replace: Boolean = false): Either[Throwable, Long]
  def del(key: String*): Either[Throwable, Long]
  def dump(key: String): Either[Throwable, Array[Byte]]
  def exists(key: String*): Either[Throwable, Long]
  def expire = ???
  def expireAt = ???
  def expireTime = ???
  def keys = ???
  def migrate = ???
  def move = ???
  def objectEncoding = ???
  def objectFreq = ???
  def objectIdleTime = ???
  def objectRefCount = ???
  def persist = ???
  def pExpire = ???
  def pExpireAt = ???
  def pExpireTime = ???
  def pTtl = ???
  def randomKey = ???
  def rename = ???
  def renameNx = ???
  def restore(key: String, serializedValue: Array[Byte], ttl: Option[Long] = Some(0), replace: Boolean = false, absTtl: Boolean = false, idleTime: Option[Long] = None, freq: Option[Long] = None): Either[Throwable, String]
  def scan = ???
  def sort = ???
  def sortRo = ???
  def touch = ???
  def ttl = ???
  def type_ = ???
  def unlink = ???
  def wait_ = ???
  def waitAof = ???
}

/**
 * </p> Note: </p>
 *   1. LCS not supported right now. </p> 2. SUBSTR is not covered because
 *      deprecated in favour of GETRANGE. </p> 3. SETEX, GETSET, SETNX, PSETEX
 *      not covered in favour of SET with related flags </p>
 */
trait StringCommands extends RedisCommandsByType {

  def append(key: String, value: String): Either[Throwable, Long]

  def set(
      key: String,
      value: String,
      ex: Option[Long] = None,
      px: Option[Long] = None,
      exat: Option[Long] = None,
      pxat: Option[Long] = None,
      nx: Boolean = false,
      xx: Boolean = false,
      keepttl: Boolean = false,
      get: Boolean = false
  ): Either[Throwable, String]
  def setRange(
      key: String,
      offset: Long,
      value: String
  ): Either[Throwable, Long]

  def get(key: String): Either[Throwable, String]
  def getDel(key: String): Either[Throwable, String]
  def getEx(
      key: String,
      ex: Option[Long] = None,
      px: Option[Long] = None,
      exat: Option[Long] = None,
      pxt: Option[Long] = None,
      persist: Boolean = false
  ): Either[Throwable, String]
  def getRange(key: String, start: Long, end: Long): Either[Throwable, String]

  def decr(key: String): Either[Throwable, Long]
  def decrBy(key: String, decrement: Long): Either[Throwable, Long]

  def incr(key: String): Either[Throwable, Long]
  def incrBy(key: String, increment: Long): Either[Throwable, Long]
  def incrByFloat(key: String, increment: Double): Either[Throwable, Double]

  def strlen(key: String): Either[Throwable, Long]

  def mget(keys: String*): Either[Throwable, Seq[Any]]
  def mset(entries: Map[String, String]): Either[Throwable, String]
  def msetNx(entries: Map[String, String]): Either[Throwable, Long]

}
