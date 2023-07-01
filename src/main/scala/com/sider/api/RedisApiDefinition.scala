package com.sider.api

import scala.util.Try
import com.sider.Type
import com.sider.SimpleError
import com.sider.BlobError
import com.sider.network.Resp3TcpClient
import com.sider.api.options.ExpireOption
import com.sider.api.entities.ScanResponse


/**
 * These API aim to be transparent as much as possible. So, until something
 * different is explicit specified by the doc all these method are just
 * utilities in order to call "plain" Redis command. Refer to Redis' commands
 * documentation for details about command's behavior.
 */
trait RedisApiDefinition {

  /* Keys commands */
  def copy(
      source: String,
      dest: String,
      db: Option[String] = None,
      replace: Boolean = false
  ): Either[Throwable, Long]
  def del(key: String*): Either[Throwable, Long]
  def dump(key: String): Either[Throwable, Array[Byte]]
  def exists(key: String*): Either[Throwable, Long]
  def expire(
      key: String,
      seconds: Long,
      option: Option[ExpireOption] = None
  ): Either[Throwable, Long]
  def expireAt(
      key: String,
      unixTimeSecond: Long,
      option: Option[ExpireOption] = None
  ): Either[Throwable, Long]
  def expireTime(key: String): Either[Throwable, Long]
  def keys(pattern: String): Either[Throwable, Seq[String]]
  def migrate = ???
  def move = ???
  def objectEncoding(key: String): Either[Throwable, String]
  def objectFreq(key: String): Either[Throwable, Long]
  def objectIdleTime(key: String): Either[Throwable, Long]
  def objectRefCount(key: String): Either[Throwable, Long]
  def persist(key: String): Either[Throwable, Long]
  def pExpire(
      key: String,
      milliseconds: Long,
      option: Option[ExpireOption] = None
  ): Either[Throwable, Long]
  def pExpireAt(
      key: String,
      unixTimeMilliseconds: Long,
      option: Option[ExpireOption] = None
  ): Either[Throwable, Long]
  def pExpireTime(key: String): Either[Throwable, Long]
  def pTtl(key: String): Either[Throwable, Long]
  def randomKey(): Either[Throwable, String]
  def rename(src: String, dest: String): Either[Throwable, String]
  def renameNx(src: String, dest: String): Either[Throwable, Long]
  def restore(
      key: String,
      serializedValue: Array[Byte],
      ttl: Option[Long] = Some(0),
      replace: Boolean = false,
      absTtl: Boolean = false,
      idleTime: Option[Long] = None,
      freq: Option[Long] = None
  ): Either[Throwable, String]
  def scan(
    cursor: String, 
    pattern: Option[String] = None,
    count: Option[Long] = None,
    _type: Option[String] = None
  ): Either[Throwable, ScanResponse]
  def sort = ???
  def sortRo = ???
  def touch(key: String*): Either[Throwable, Long]
  def ttl(key: String): Either[Throwable, Long]
  def type_(key: String): Either[Throwable, String]
  def unlink(key: String*): Either[Throwable, Long]
  def wait_ = ???
  def waitAof = ???

  /*
    * Strings commands:
    * </p> Note: </p>
    *   1. LCS not supported right now. </p> 2. SUBSTR is not covered because
    *      deprecated in favour of GETRANGE. </p> 3. SETEX, GETSET, SETNX, PSETEX
    *      not covered in favour of SET with related flags </p>
    */

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
