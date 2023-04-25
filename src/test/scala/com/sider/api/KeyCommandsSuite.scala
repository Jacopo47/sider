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

class KeyCommandsSuite extends munit.FunSuite {

  val logger = LoggerFactory.getLogger(classOf[KeyCommandsSuite])

  val redisServer = new RedisServer()
    .withExposedPorts(6379)

  override val munitTimeout = Duration(15, "s")

  override def beforeAll(): Unit = redisServer.start()

  override def afterAll(): Unit = redisServer.stop()

  test("EXISTS, COPY, DEL; TOUCH") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.keys

    c.strings.set("foo", "bar")
    assertEquals(cmd.exists("foo"), Right(1L))
    assertEquals(cmd.copy("foo", "foo:1"), Right(1L))
    assertEquals(cmd.copy("foo", "foo:1"), Right(0L))
    assertEquals(cmd.copy("foo", "foo:1", replace = true), Right(1L))
    assertEquals(cmd.touch("foo", "foo:1", "not_exists"), Right(2L))
    assertEquals(cmd.del("foo", "foo:1"), Right(2L))
    assertEquals(cmd.del("foo", "foo:1"), Right(0L))
  }

  test("DUMP, SERIALIZATION") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.keys

    c.strings.set("dump:foo", "bar")
    val dumped = cmd.dump("dump:foo")
    assertEquals(cmd.del("dump:foo"), Right(1L))
    assertEquals(cmd.restore("dump:foo", dumped.getOrElse(Array.empty[Byte])), Right("OK"))
    assertEquals(c.strings.get("dump:foo"), Right("bar"))

    val dumped1 = cmd.dump("dump:foo").getOrElse(Array.empty[Byte])
    assertEquals(cmd.del("dump:foo"), Right(1L))
    
    assertEquals(cmd.restore("dump:foo", dumped1, ttl = Some(300L), replace = true, freq = Some(200L)),Right("OK"))
    assertEquals(c.strings.get("dump:foo"), Right("bar"))
  }

  test("EXPIRE* , TTL") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.keys

    c.strings.set("expire:foo", "bar")
    assertEquals(cmd.ttl("expire:foo"), Right(-1L))
    assertEquals(cmd.ttl("expire:foo"), Right(-1L))
    assertEquals(cmd.ttl("expire:not_exists"), Right(-2L))
    assertEquals(cmd.ttl("expire:not_exists"), Right(-2L))

    c.keys.expire("expire:foo", Int.MaxValue.toLong)
    assert(cmd.ttl("expire:foo").exists(_ > 0))
    assert(cmd.pTtl("expire:foo").exists(_ > 0))

    
    assertEquals(cmd.persist("expire:foo"), Right(1L))
    assert(cmd.ttl("expire:foo").exists(_ == -1))
    assert(cmd.pTtl("expire:foo").exists(_ == -1))

    c.keys.pExpire("expire:foo", Int.MaxValue.toLong)
    assert(cmd.ttl("expire:foo").exists(_ > 0))
    assert(cmd.pTtl("expire:foo").exists(_ > 0))

    val toNextMonth = Instant.now().plus(30, ChronoUnit.DAYS).getEpochSecond()
    c.keys.expireAt("expire:foo", toNextMonth, Some(com.sider.api.options.XX()))
    assertEquals(cmd.expireTime("expire:foo"), Right(toNextMonth))

    assertEquals(cmd.persist("expire:foo"), Right(1L))

    c.keys.pExpireAt("expire:foo", toNextMonth * 1000)
    assertEquals(cmd.pExpireTime("expire:foo"), Right(toNextMonth * 1000))
  }

  test("KEYS") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.keys

    c.strings.mset(Map("keys:a" -> "a", "keys:b" -> "b", "keys:c" -> "c", "keys-with-different-pattern:d" -> "d", "keys-with-different-pattern:?" -> "d"))

    assertEquals(cmd.keys("keys*").map(_.size), Right(5))
    assertEquals(cmd.keys("keys:*").map(_.size), Right(3))
    assertEquals(cmd.keys("keys-with-different-pattern:?").map(_.size), Right(2))
    assertEquals(cmd.keys("keys-with-different-pattern:\\?").map(_.size), Right(1)) 
  }

  test("OBJECT*") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.keys

    c.strings.set("object:a", "a")

    assertEquals(cmd.objectEncoding("object:a"), Right("embstr"))
    // OBJECT FREQ works only under specific configuration about LFU policies. This is why left is accepted in this test scenario
    assert(cmd.objectFreq("object:a").isLeft)
    val app = cmd.objectIdleTime("object:a")
    assert(cmd.objectIdleTime("object:a").isRight)
    assertEquals(cmd.objectRefCount("object:a"), Right(1L))
  }

  test("RANDOM, RENAME*") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.keys

    c.strings.set("rename:a", "a")

    assert(cmd.randomKey().isRight)

    assertEquals(cmd.rename("rename:a", "rename:b"), Right("OK"))
    assertEquals(cmd.exists("rename:a"), Right(0L))
    assertEquals(cmd.renameNx("rename:b", "rename:a"), Right(1L))
    assertEquals(cmd.exists("rename:a"), Right(1L))    
  }

  test("SCAN") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.keys

    /* Prepares a list of keys for this test scenario */
    (1 to 100) map(_.toString()) map { e => c.strings.set(s"scan:$e", e) }

    /* Utility functions that can be used to do a complete scan */
    def scanAll(start: String, count: Int): Int = {
      if ("0".equals(start) && count >= 0) then return count


      logger.debug("Executing scan..")
      val res = cmd.scan(start, Some("scan:*"),  Some(10L), Some("string")).getOrElse(ScanResponse("0", Seq.empty))
      scanAll(res.next, (if (count == -1) then 0 else count) + res.elements.size)
    }

    assertEquals(scanAll("0", -1), 100)
  }
  
}
