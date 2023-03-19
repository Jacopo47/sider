package com.sider.network

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.net.Socket
import scala.util.Try
import java.io.InputStream
import com.sider.Identifiers
import com.sider.Simple
import com.sider.Resp3Serialization.RN
import com.sider.Resp3Serialization.R
import com.sider.Resp3Serialization.N
import com.sider.Resp3Serialization
import com.sider.bytesToLong
import com.sider.Resp3Serialization.RNs
import com.sider.Resp3Serialization.RNa
import scala.annotation.tailrec
import com.sider.Complex
import com.sider.Aggregate
import com.sider.Type
import java.io.ByteArrayInputStream
import java.io.InputStreamReader

class Resp3TcpClient(
    val host: Option[String] = Some("localhost"),
    val port: Option[Int] = Some(6379)
) extends AutoCloseable {

  override def close(): Unit = socket map { _.close() }

  val logger: Logger = LoggerFactory.getLogger(classOf[Resp3TcpClient])

  lazy val socket: Either[Throwable, Socket] = (host, port) match {
    case (Some(h), Some(p)) =>
      logger.debug("Opening connection {}:{}", h, p); Right(new Socket(h, p))
    case _ => Left(Throwable("Host and/or port not valid"))
  }

  def gossip(input: Array[Byte]): Either[Throwable, Type[?]] =
    for {
      s <- socket
      _ <- Right(logger.debug("Sending bytes {}", input))
      _ <- Right(s.getOutputStream().write(input))
      _ <- Right(s.getOutputStream().flush())
      _ <- Right(logger.debug("Listening for bytes.."))
      res <- collectResponseToResp3Type(s.getInputStream())
      _ <- Right(logger.debug("Here some bytes {}", res))
    } yield res

  def gossip(input: String): Either[Throwable, Type[?]] =
    this.gossip(Resp3Serialization.toCommand(input))

  def collectResponseToResp3Type(input: InputStream): Either[Throwable, Type[?]] = {
    Resp3Serialization.read(
      LazyList.continually({
        val next = input.read().toByte
        logger.debug("Read {}", next)

        next
      })
    )
  }

}