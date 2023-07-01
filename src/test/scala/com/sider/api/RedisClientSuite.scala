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

    val app = c.api.set("foo", "bar").get

    assertEquals(c.api.get("foo").get, Right("bar"))

    assertEquals(c.api.get("foo1").get, Left(KeyNotFound()))
    assertEquals(c.api.set("foo1", "bar").get, Right("OK"))
    assertEquals(c.api.get("foo1").get, Right("bar"))

    assertEquals(c.api.getDel("foo1").get, Right("bar"))
    assertEquals(c.api.getDel("foo1").get, Left(KeyNotFound()))

    assertEquals(cmd.set("getex:foo", "Hello World", ex = Some(360L)).get, Right("OK"))
    assertEquals(cmd.getEx("getex:foo").get, Right("Hello World"))
    assertEquals(cmd.getEx("getex:foo", ex = Some(120L)).get, Right("Hello World"))
    assertEquals(cmd.getEx("getex:foo", px = Some(120000L)).get, Right("Hello World"))   
    assert(cmd.getEx("getex:foo", ex = Some(120L), px = Some(120000L)).get.isLeft)   
  }

  test("INCR* AND DECR*") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))

    assert(c.api.set("foo", "0").get.isRight)
    assertEquals(c.api.incr("foo").get, Right(1L))
    assertEquals(c.api.incrBy("foo", 19L).get, Right(20L))
    assertEquals(c.api.incrByFloat("foo", 1.2).get, Right(21.2))
    assertEquals(c.api.incrByFloat("foo", -21.2).get, Right(0.0))
    assertEquals(c.api.get("foo").get, Right("0"))

    assertEquals(c.api.decr("foo").get, Right(-1L))
    assertEquals(c.api.decrBy("foo", 1L).get, Right(-2L))
    assertEquals(c.api.decrBy("foo", -22L).get, Right(20L))
  }

  test("APPEND") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    assertEquals(cmd.append("append:foo", "Hello").get, Right(5L))
    assertEquals(cmd.append("append:foo", " World").get, Right(11L))
    assertEquals(cmd.get("append:foo").get, Right("Hello World"))
  }

  test("STRLEN") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    assertEquals(cmd.set("strlen:foo", "Hello world").get, Right("OK"))
    assertEquals(cmd.strlen("strlen:foo").get, Right(11L))
    assertEquals(cmd.strlen("nonexisting").get, Right(0L))
  }

  test("MSET AND MGET") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    assert(cmd.mset(Map.empty).get.isLeft)
    assertEquals(cmd.mset(Map("mset:a" -> "a", "mset:b" -> "b")).get, Right("OK"))
    assertEquals(cmd.get("mset:a").get, Right("a"))
    assertEquals(cmd.get("mset:b").get, Right("b"))

    assertEquals(cmd.mget("mset:a", "mset:b").get, Right(Seq("a", "b")))
    assertEquals(cmd.mget("mset:a", "mset:b", "nonexisting").get, Right(Seq("a", "b", null)))


    assertEquals(cmd.msetNx(Map("msetnx:a" -> "a", "msetnx:b" -> "b")).get, Right(1L))
    assertEquals(cmd.msetNx(Map("msetnx:a" -> "a", "msetnx:c" -> "c")).get, Right(0L))
  }


  test("SET/GET RANGE") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    assertEquals(cmd.set("range:a", "Hello World").get, Right("OK"))
    assertEquals(cmd.setRange("range:a", 6, "Redis").get, Right(11L))
    assertEquals(cmd.get("range:a").get, Right("Hello Redis"))

    assertEquals(cmd.setRange("range:b", 6, "Redis").get, Right(11L))

    assertEquals(cmd.getRange("range:a", 0L, -1L).get, Right("Hello Redis"))
    assertEquals(cmd.getRange("range:a", 100L, -1L).get, Right(""))
    assertEquals(cmd.getRange("nonexisting", 0L, -1L).get, Right(""))
  }
}
