package com.sider.network

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import fs2.{Chunk, Stream}
import fs2.io.net.Network
import cats.effect.MonadCancelThrow
import cats.effect.std.Console
import cats.syntax.all._
import com.comcast.ip4s._
import cats.effect.IO
import fs2.text
import cats.effect.kernel.MonadCancel

class Fs2Client(
    val host: Option[String] = Some("localhost"),
    val port: Option[Int] = Some(6379)
) {

  val logger: Logger = LoggerFactory.getLogger(classOf[Fs2Client])

  def client[F[_]: MonadCancelThrow: Console: Network]: Stream[F, Unit] = {
    (
      host flatMap { Host.fromString(_) },
      port flatMap { Port.fromInt(_) }
    ) match {
      case (Some(h), Some(p)) => {
        Stream.resource(Network[F].client(SocketAddress(h, p))).flatMap {
          socket =>
            Stream("*1\r\n$4\r\nPING\r\n")
              .through(text.utf8.encode)
              .through(socket.writes) ++
              socket.reads
                .through(text.utf8.decode)
                .through(text.lines)
                .head
                .foreach { response =>
                  Console[F].println(s"Response: $response")
                }
        }
      }

      case _ => {
        Stream.eval(Console[F].println("Invalid"))
      }
    }
  }

}
