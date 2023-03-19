package com.sider.network

import org.testcontainers.containers.GenericContainer
import org.testcontainers.utility.DockerImageName
import org.testcontainers.containers.wait.strategy.Wait
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import com.sider.toSeqOfBytes
import com.sider.bytesToString
import com.sider.Serialization.RNs
import com.sider.toStream
import com.sider.SimpleString

class RedisServer
    extends GenericContainer[RedisServer](DockerImageName.parse("redis")) {
  self().waitingFor(
    Wait.forLogMessage(".*Ready to accept connections.*\\n", 1)
  );

}

class TCPClientSuite extends munit.FunSuite {

  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global
  val redisServer = new RedisServer()
    .withExposedPorts(6379)

  override val munitTimeout = Duration(15, "s")

  // override def beforeAll(): Unit = redisServer.start()

  test("Basic commands") {
    val client = new TCPClient(Some("localhost"), Some(6379))
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
  }
}
