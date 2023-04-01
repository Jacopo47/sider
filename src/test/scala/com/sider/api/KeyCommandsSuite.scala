package com.sider.api

import com.sider.network.RedisServer
import scala.concurrent.duration.Duration

class KeyCommandsSuite extends munit.FunSuite {

  val redisServer = new RedisServer()
    .withExposedPorts(6379)

  override val munitTimeout = Duration(15, "s")

  override def beforeAll(): Unit = redisServer.start()

  override def afterAll(): Unit = redisServer.stop()

  test("EXISTS, COPY, DEL") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.keys

    c.strings.set("foo", "bar")
    assertEquals(cmd.exists("foo"), Right(1L))
    assertEquals(cmd.copy("foo", "foo:1"), Right(1L))
    assertEquals(cmd.copy("foo", "foo:1"), Right(0L))
    assertEquals(cmd.copy("foo", "foo:1", replace = true), Right(1L))
    assertEquals(cmd.del("foo", "foo:1"), Right(2L))
    assertEquals(cmd.del("foo", "foo:1"), Right(0L))
  }

  test("DUMP, SERIALIZATION") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.keys

    c.strings.set("dump:foo", "bar")
    val dumped = cmd.dump("dump:foo")
    assertEquals(cmd.del("dump:foo"), Right(1L))
    assertEquals(cmd.restore("dump:foo", dumped.getOrElse("")), Right("OK"))
    assertEquals(c.strings.get("dump:foo"), Right("bar"))

  }
  
}
