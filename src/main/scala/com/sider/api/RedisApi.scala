package com.sider.api

import scala.util.Try

trait RedisApi {
  val strings: StringCommands
}

sealed trait RedisCommandsByType {}

/**
 * These API aim to be transparent as much as possible. 
 * So, until something different is explicit specified by the doc all these method are just utilities in order to call
 * "plain" Redis command. Refer to Redis' commands documentation for details about command's behavior.
 * 
 * </p>
 * Note: </p>
 * 1. LCS not supported right now. </p>
 * 2. SUBSTR is not covered because deprecated in favour of GETRANGE. </p>
 * 3. SETEX, GETSET, SETNX, PSETEX not covered in favour of SET with related flags </p>
 */
trait StringCommands extends RedisCommandsByType {

  def append(key: String, value: String): Either[Throwable, String]

  def set(
      key: String,
      value: String,
      ex: Option[Long] = None,
      px: Option[Long] = None,
      exat: Option[Long] = None,
      nx: Boolean = false,
      xx: Boolean = false,
      keepttl: Boolean = false,
      get: Boolean = false
  ): Either[Throwable, String]
  def setRange(key: String, offset: Long, value: String): Either[Throwable, Long]

  def get(key: String): Either[Throwable, String]
  def getDel(key: String): Either[Throwable, String]
  def getEx(
      key: String,
      ex: Option[Long] = None,
      px: Option[Long] = None,
      exat: Option[Long] = None,
      paxt: Option[Long] = None,
      persist: Boolean = false
  ): Either[Throwable, String]
  def getRange(key: String, start: Long, end: Long): Either[Throwable, String]

  def decr(key: String): Either[Throwable, Long]
  def decrBy(key: String, decrement: Long): Either[Throwable, Long]

  def incr(key: String): Either[Throwable, String]
  def incrBy(key: String, increment: Long): Either[Throwable, String]
  def incrByFloat(key: String, increment: Double): Either[Throwable, String]

  def strlen(key: String): Either[Throwable, Long]

  def mget(keys: String*): Either[Throwable, Seq[String]]
  def mset(entries: Map[String,String]): Either[Throwable, String]
  def msetNx(entries: Map[String, String]): Either[Throwable, Long]

}
