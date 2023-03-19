package com.sider.network

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.net.Socket
import scala.util.Try
import java.io.InputStream
import com.sider.Identifiers
import com.sider.Simple
import com.sider.Serialization.RN
import com.sider.Serialization.R
import com.sider.Serialization.N
import com.sider.network.TCPClient.endOfStreamByte
import com.sider.Serialization
import com.sider.bytesToLong
import com.sider.Serialization.RNs
import com.sider.Serialization.RNa
import scala.annotation.tailrec
import com.sider.Complex
import com.sider.Aggregate
import com.sider.Type
import java.io.ByteArrayInputStream
import java.io.InputStreamReader

class TCPClient(
    val host: Option[String] = Some("localhost"),
    val port: Option[Int] = Some(6379)
) extends AutoCloseable {

  override def close(): Unit = socket map { _.close() }

  val logger: Logger = LoggerFactory.getLogger(classOf[TCPClient])

  lazy val socket: Either[Throwable, Socket] = (host, port) match {
    case (Some(h), Some(p)) =>
      logger.debug("Opening connection {}:{}", h, p); Right(new Socket(h, p))
    case _ => Left(Throwable("Host and/or port not valid"))
  }

  def gossip(input: Array[Byte]): Either[Throwable, Type[?]] =
    for {
      s <- socket
      _ <- Right(logger.debug("Sending bytes {}", input))
      _ <- Right(s.getOutputStream().write(input.toArray))
      _ <- Right(s.getOutputStream().flush())
      _ <- Right(logger.debug("Listening for bytes.."))
      res <- responseToType(s.getInputStream())
      _ <- Right(logger.debug("Here some bytes {}", res))
    } yield res

  def gossip(input: String): Either[Throwable, Type[?]] =
    this.gossip(Serialization.toResp3Command(input))

  def responseToType(input: InputStream): Either[Throwable, Type[?]] = {
    Serialization.read(
      LazyList.continually({
        val next = input.read().toByte
        logger.debug("Read {}", next)

        next
      })
    )
  }

}

object TCPClient:
  val endOfStreamByte = -1.toByte
