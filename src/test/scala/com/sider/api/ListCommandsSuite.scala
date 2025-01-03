package com.sider.api

import org.slf4j.LoggerFactory
import com.sider.network.RedisServer
import scala.concurrent.duration.Duration
import com.sider.api.implementation.NotExistingOrEmptyList

class ListCommandsSuite extends munit.FunSuite {

  val logger = LoggerFactory.getLogger(classOf[KeyCommandsSuite])

  val redisServer = new RedisServer()
    .withExposedPorts(6379)

  override val munitTimeout = Duration(15, "s")

  override def beforeAll(): Unit = redisServer.start()

  override def afterAll(): Unit = redisServer.stop()


  
  test("Basic list commands") {
    val c = new RedisClient(port = Some(redisServer.getMappedPort(6379)))
    val cmd = c.api

    assert(cmd.lpush("list:a").isLeft)
    assertEquals(cmd.lpush("list:a", "1", "2"), Right(2L))
    assertEquals(cmd.llen("list:a"), Right(2L))
    assertEquals(cmd.lpush("list:a", "3", "4"), Right(4L))
    assertEquals(cmd.llen("list:a"), Right(4L))


    assertEquals(cmd.lpop("list:a"), Right(Seq("4")))
    assertEquals(cmd.lpop("list:a"), Right(Seq("3")))
    assertEquals(cmd.lpop("list:a"), Right(Seq("2")))
    assertEquals(cmd.lpop("list:a"), Right(Seq("1")))
    assertEquals(cmd.lpop("list:a"), Left(NotExistingOrEmptyList("Key list:a does not exist or is an empty list")))
    assertEquals(cmd.llen("list:a"), Right(0L))


    assertEquals(cmd.lpush("list:a", "1", "2", "3", "4"), Right(4L))
    assertEquals(cmd.llen("list:a"), Right(4L))
    assertEquals(cmd.lpop("list:a", Some(4)), Right(Seq("4", "3", "2", "1")))
    assertEquals(cmd.llen("list:a"), Right(0L))

  }
}
