package com.sider.api

import scala.concurrent.duration.Duration
import com.sider.network.RedisServer

class RedisClientSuite extends munit.FunSuite {

  val redisServer = new RedisServer()
    .withExposedPorts(6379)

  override val munitTimeout = Duration(15, "s")

  override def beforeAll(): Unit = redisServer.start()

  override def afterAll(): Unit = redisServer.stop()

  test("SET and GET") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    val app = c.api.set("foo", "bar")

    assertEquals(c.api.get("foo"), Right("bar"))

    assertEquals(c.api.get("foo1"), Left(KeyNotFound()))
    assertEquals(c.api.set("foo1", "bar"), Right("OK"))
    assertEquals(c.api.get("foo1"), Right("bar"))

    assertEquals(c.api.getDel("foo1"), Right("bar"))
    assertEquals(c.api.getDel("foo1"), Left(KeyNotFound()))

    assertEquals(cmd.set("getex:foo", "Hello World", ex = Some(360L)), Right("OK"))
    assertEquals(cmd.getEx("getex:foo"), Right("Hello World"))
    assertEquals(cmd.getEx("getex:foo", ex = Some(120L)), Right("Hello World"))
    assertEquals(cmd.getEx("getex:foo", px = Some(120000L)), Right("Hello World"))   
    assert(cmd.getEx("getex:foo", ex = Some(120L), px = Some(120000L)).isLeft)   
  }

  test("INCR* AND DECR*") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))

    assert(c.api.set("foo", "0").isRight)
    assertEquals(c.api.incr("foo"), Right(1L))
    assertEquals(c.api.incrBy("foo", 19L), Right(20L))
    assertEquals(c.api.incrByFloat("foo", 1.2), Right(21.2))
    assertEquals(c.api.incrByFloat("foo", -21.2), Right(0.0))
    assertEquals(c.api.get("foo"), Right("0"))

    assertEquals(c.api.decr("foo"), Right(-1L))
    assertEquals(c.api.decrBy("foo", 1L), Right(-2L))
    assertEquals(c.api.decrBy("foo", -22L), Right(20L))
  }

  test("APPEND") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    assertEquals(cmd.append("append:foo", "Hello"), Right(5L))
    assertEquals(cmd.append("append:foo", " World"), Right(11L))
    assertEquals(cmd.get("append:foo"), Right("Hello World"))
  }

  test("STRLEN") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    assertEquals(cmd.set("strlen:foo", "Hello world"), Right("OK"))
    assertEquals(cmd.strlen("strlen:foo"), Right(11L))
    assertEquals(cmd.strlen("nonexisting"), Right(0L))
  }

  test("MSET AND MGET") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    assert(cmd.mset(Map.empty).isLeft)
    assertEquals(cmd.mset(Map("mset:a" -> "a", "mset:b" -> "b")), Right("OK"))
    assertEquals(cmd.get("mset:a"), Right("a"))
    assertEquals(cmd.get("mset:b"), Right("b"))

    assertEquals(cmd.mget("mset:a", "mset:b"), Right(Seq("a", "b")))
    assertEquals(cmd.mget("mset:a", "mset:b", "nonexisting"), Right(Seq("a", "b", null)))


    assertEquals(cmd.msetNx(Map("msetnx:a" -> "a", "msetnx:b" -> "b")), Right(1L))
    assertEquals(cmd.msetNx(Map("msetnx:a" -> "a", "msetnx:c" -> "c")), Right(0L))
  }


  test("SET/GET RANGE") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    assertEquals(cmd.set("range:a", "Hello World"), Right("OK"))
    assertEquals(cmd.setRange("range:a", 6, "Redis"), Right(11L))
    assertEquals(cmd.get("range:a"), Right("Hello Redis"))

    assertEquals(cmd.setRange("range:b", 6, "Redis"), Right(11L))

    assertEquals(cmd.getRange("range:a", 0L, -1L), Right("Hello Redis"))
    assertEquals(cmd.getRange("range:a", 100L, -1L), Right(""))
    assertEquals(cmd.getRange("nonexisting", 0L, -1L), Right(""))
  }
}
