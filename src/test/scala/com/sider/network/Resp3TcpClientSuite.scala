package com.sider.network

import org.testcontainers.containers.GenericContainer
import org.testcontainers.utility.DockerImageName
import org.testcontainers.containers.wait.strategy.Wait
import scala.concurrent.duration.Duration
import com.sider.SimpleString
import com.sider.Resp3Serialization.RNs

class RedisServer
    extends GenericContainer[RedisServer](DockerImageName.parse("redis")) {
  self().waitingFor(
    Wait.forLogMessage(".*Ready to accept connections.*\\n", 1)
  );

}

class Resp3TcpClientSuite extends munit.FunSuite {

  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global
  val redisServer = new RedisServer()
    .withExposedPorts(6379)

  override val munitTimeout = Duration(15, "s")

  override def beforeAll(): Unit = redisServer.start()

  override def afterAll(): Unit = redisServer.stop()

  test("Basic commands") {
    val client = new Resp3TcpClient(Some("localhost"), Some(redisServer.getMappedPort(6379)))
    client.gossip("PING") match {
      case v: Either[Throwable, SimpleString] =>
        assertEquals(v flatMap { _.value }, Right("PONG"))
      case _ => assert(false)

    }

    assertEquals(
      client.gossip("SET FOO bar") flatMap { _.value },
      Right(s"OK")
    )
    assertEquals(
      client.gossip("GET FOO") flatMap { _.value },
      Right("bar")
    )

    assertEquals(
      client.gossip(s"SET withNewLine foo${RNs}bar") flatMap { _.value },
      Right("OK")
    )

    assertEquals(
      client.gossip("GET withNewLine") flatMap { _.value },
      Right(s"foo${RNs}bar")
    )

    assertEquals(
      client.gossip(s"RPUSH mylist 1 2 3") flatMap { _.value },
      Right(3)
    )

    assertEquals(
      client.gossip(s"LRANGE mylist 0 -1") flatMap { _.value },
      Right(Seq("1", "2", "3"))
    )

    assertEquals(
      client.gossip(s"RPUSH mylist 4 5 6") flatMap { _.value },
      Right(6)
    )

    assertEquals(
      client.gossip(s"LRANGE mylist 0 -1") flatMap { _.value },
      Right(Seq("1", "2", "3", "4", "5", "6"))
    )
  }
}
