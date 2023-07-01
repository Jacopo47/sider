package com.sider.api

import com.sider.network.RedisServer
import scala.concurrent.duration.Duration
import java.util.Date
import java.time.Instant
import java.util.Calendar
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import com.sider.api.entities.ScanResponse
import com.sider.Resp3Serialization.logger
import org.slf4j.LoggerFactory
import com.sider.concurrency.IO

class KeyCommandsSuite extends munit.FunSuite {

  val logger = LoggerFactory.getLogger(classOf[KeyCommandsSuite])

  val redisServer = new RedisServer()
    .withExposedPorts(6379)

  override val munitTimeout = Duration(15, "s")

  override def beforeAll(): Unit = redisServer.start()

  override def afterAll(): Unit = redisServer.stop()

  test("EXISTS, COPY, DEL, TOUCH, TYPE, UNLINK") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    c.api.set("foo", "bar").get
    assertEquals(cmd.exists("foo").get, Right(1L))
    assertEquals(cmd.copy("foo", "foo:1").get, Right(1L))
    assertEquals(cmd.copy("foo", "foo:1").get, Right(0L))
    assertEquals(cmd.copy("foo", "foo:1", replace = true).get, Right(1L))
    assertEquals(cmd.touch("foo", "foo:1", "not_exists").get, Right(2L))
    assertEquals(cmd.del("foo", "foo:1").get, Right(2L))
    assertEquals(cmd.del("foo", "foo:1").get, Right(0L))

    c.api.set("foo", "bar").get
    assertEquals(cmd.type_("foo").get, Right("string"))
    assertEquals(cmd.unlink("foo", "not_exists").get, Right(1L))
  }

  test("DUMP, SERIALIZATION") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    c.api.set("dump:foo", "bar").get
    val dumped = cmd.dump("dump:foo").get
    assertEquals(cmd.del("dump:foo").get, Right(1L))
    assertEquals(cmd.restore("dump:foo", dumped.getOrElse(Array.empty[Byte])).get, Right("OK"))
    assertEquals(c.api.get("dump:foo").get, Right("bar"))

    val dumped1 = cmd.dump("dump:foo").get.getOrElse(Array.empty[Byte])
    assertEquals(cmd.del("dump:foo").get, Right(1L))
    
    assertEquals(cmd.restore("dump:foo", dumped1, ttl = Some(300L), replace = true, freq = Some(200L)).get, Right("OK"))
    assertEquals(c.api.get("dump:foo").get, Right("bar"))
  }

  test("EXPIRE* , TTL") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    c.api.set("expire:foo", "bar").get
    assertEquals(cmd.ttl("expire:foo").get, Right(-1L))
    assertEquals(cmd.ttl("expire:foo").get, Right(-1L))
    assertEquals(cmd.ttl("expire:not_exists").get, Right(-2L))
    assertEquals(cmd.ttl("expire:not_exists").get, Right(-2L))

    c.api.expire("expire:foo", Int.MaxValue.toLong).get
    assert(cmd.ttl("expire:foo").get.exists(_ > 0))
    assert(cmd.pTtl("expire:foo").get.exists(_ > 0))

    
    assertEquals(cmd.persist("expire:foo").get, Right(1L))
    assert(cmd.ttl("expire:foo").get.exists(_ == -1))
    assert(cmd.pTtl("expire:foo").get.exists(_ == -1))

    c.api.pExpire("expire:foo", Int.MaxValue.toLong).get
    assert(cmd.ttl("expire:foo").get.exists(_ > 0))
    assert(cmd.pTtl("expire:foo").get.exists(_ > 0))

    val toNextMonth = Instant.now().plus(30, ChronoUnit.DAYS).getEpochSecond()
    c.api.expireAt("expire:foo", toNextMonth, Some(com.sider.api.options.XX())).get
    assertEquals(cmd.expireTime("expire:foo").get, Right(toNextMonth))

    assertEquals(cmd.persist("expire:foo").get, Right(1L))

    c.api.pExpireAt("expire:foo", toNextMonth * 1000).get
    assertEquals(cmd.pExpireTime("expire:foo").get, Right(toNextMonth * 1000))
  }

  test("KEYS") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    c.api.mset(Map("keys:a" -> "a", "keys:b" -> "b", "keys:c" -> "c", "keys-with-different-pattern:d" -> "d", "keys-with-different-pattern:?" -> "d")).get

    assertEquals(cmd.keys("keys*").get.map(_.size), Right(5))
    assertEquals(cmd.keys("keys:*").get.map(_.size), Right(3))
    assertEquals(cmd.keys("keys-with-different-pattern:?").get.map(_.size), Right(2))
    assertEquals(cmd.keys("keys-with-different-pattern:\\?").get.map(_.size), Right(1)) 
  }

  test("OBJECT*") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    c.api.set("object:a", "a").get

    assertEquals(cmd.objectEncoding("object:a").get, Right("embstr"))
    // OBJECT FREQ works only under specific configuration about LFU policies. This is why left is accepted in this test scenario
    assert(cmd.objectFreq("object:a").get.isLeft)
    val app = cmd.objectIdleTime("object:a")
    assert(cmd.objectIdleTime("object:a").get.isRight)
    assertEquals(cmd.objectRefCount("object:a").get, Right(1L))
  }

  test("RANDOM, RENAME*") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    c.api.set("rename:a", "a").get

    assert(cmd.randomKey().get.isRight)

    assertEquals(cmd.rename("rename:a", "rename:b").get, Right("OK"))
    assertEquals(cmd.exists("rename:a").get, Right(0L))
    assertEquals(cmd.renameNx("rename:b", "rename:a").get, Right(1L))
    assertEquals(cmd.exists("rename:a").get, Right(1L))    
  }

  test("SCAN") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    /* Prepares a list of keys for this test scenario */
    (1 to 100) map(_.toString()) map { e => c.api.set(s"scan:$e", e).get }

    /* Utility functions that can be used to do a complete scan */
    def scanAll(start: String, count: Int): Int = {
      if ("0".equals(start) && count >= 0) then return count


      logger.debug("Executing scan..")
      val res = cmd.scan(start, Some("scan:*"),  Some(10L), Some("string")).get.getOrElse(ScanResponse("0", Seq.empty))
      scanAll(res.next, (if (count == -1) then 0 else count) + res.elements.size)
    }

    assertEquals(scanAll("0", -1), 100)
  }
  
}
