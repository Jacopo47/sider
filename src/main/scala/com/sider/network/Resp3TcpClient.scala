package com.sider.network

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.net.Socket
import com.sider.Type
import com.sider.Resp3Serialization
import java.io.InputStream
import com.sider.concurrency.IO

class Resp3TcpClient(
    val host: Option[String],
    val port: Option[Int]
) extends AutoCloseable {

  override def close(): Unit = socket map { _.close() }

  val logger: Logger = LoggerFactory.getLogger(classOf[Resp3TcpClient])

  lazy val socket: Either[Throwable, Socket] = (host, port) match {
    case (Some(h), Some(p)) =>
      logger.debug("Opening connection {}:{}", h, p)
      Right(new Socket(h, p))
    case _ => Left(Throwable("Host and/or port not valid"))
  } map( s => {

    // Gossip in order to register the client with RESP3 Protocol
    s.getOutputStream().write(Resp3Serialization.toCommand("HELLO 3"))
    s.getOutputStream().flush()

    val input = s.getInputStream()
    val res = collectResponseToResp3Type(s.getInputStream())

    s
  })

  def sendAndWaitResponse(
      input: Array[Byte]
  ): IO[Either[Throwable, Type[?]]] =
    IO {
      for {
        s <- socket
        _ <- Right(logger.debug("Sending bytes {}", input))
        _ <- Right(s.getOutputStream().write(input))
        _ <- Right(s.getOutputStream().flush())
        _ <- Right(logger.debug("Listening for bytes.."))
        res <- collectResponseToResp3Type(s.getInputStream())
        _ <- Right(logger.debug("Here some bytes {}", res))
      } yield res
    }

  def sendAndWaitResponse(input: String): IO[Either[Throwable, Type[?]]] =
    this.sendAndWaitResponse(Resp3Serialization.toCommand(input))

  def sendAndWaitResponseSync(input: String): Either[Throwable, Type[?]] =
    this.sendAndWaitResponse(input).get

  def collectResponseToResp3Type(
      input: InputStream
  ): Either[Throwable, Type[?]] = {
    Resp3Serialization.read(
      LazyList.continually {
        val next = input.read().toByte
        logger.debug("Read {}", next)

        next
      }
    )
  }

}
