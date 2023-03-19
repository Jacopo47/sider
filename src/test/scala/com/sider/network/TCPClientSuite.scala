package com.sider.network

import org.testcontainers.containers.GenericContainer
import org.testcontainers.utility.DockerImageName
import org.testcontainers.containers.wait.strategy.Wait
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import com.sider.toSeqOfBytes
import com.sider.bytesToString
import com.sider.Serialization.RNs

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
    Future {
      val client = new TCPClient(Some("localhost"), Some(6379))
      val res = client.gossip("PING")
      assertEquals(res map { _.bytesToString }, Right(s"+PONG$RNs"))

      assertEquals(client.gossip("SET FOO bar") map { _.bytesToString }, Right(s"+OK$RNs"))

      assertEquals(client.gossip("GET FOO") map { _.bytesToString }, Right(s"$$3${RNs}bar$RNs"))
    }
  }
}
