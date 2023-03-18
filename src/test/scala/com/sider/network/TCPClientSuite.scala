package com.sider.network

import org.testcontainers.containers.GenericContainer
import org.testcontainers.utility.DockerImageName
import org.testcontainers.containers.wait.strategy.Wait
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import cats.effect.IO.asyncForIO
import cats.effect.IO
import cats.effect.unsafe.implicits.global

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

  //override def beforeAll(): Unit = redisServer.start()

  test("Echo") {
      new Fs2Client(Some("localhost"), Some(6379))
        .client.compile.drain.unsafeRunSync()

      assert(true)

  }
}
