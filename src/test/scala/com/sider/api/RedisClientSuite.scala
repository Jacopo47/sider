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

    c.strings.set("foo", "bar")

    assertEquals(c.strings.get("foo").getOrElse("Error"), "bar")

    // TODO : does not work because SET is not send and response is not proper parsed
    //c.strings.set("foo1", "bar")
    val res = c.strings.get("foo1")
    assertEquals(res, Left(KeyNotFound()))  

    c.strings.set("foo1", "bar")
    assertEquals(c.strings.get("foo1"), Right("bar"))  
  }
}
