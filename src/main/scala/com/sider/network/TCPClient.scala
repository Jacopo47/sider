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

class TCPClient(
    val host: Option[String] = Some("localhost"),
    val port: Option[Int] = Some(6379)
) extends AutoCloseable {


  override def close(): Unit = socket map { _.close() }

  val logger: Logger = LoggerFactory.getLogger(classOf[TCPClient])

  lazy val socket: Either[Throwable, Socket] = (host, port) match {
    case (Some(h), Some(p)) => logger.debug("Opening connection {}:{}", h, p); Right(new Socket(h, p))
    case _                  => Left(Throwable("Host and/or port not valid"))
  }

  def gossip(input: Seq[Byte]): Either[Throwable, Seq[Byte]] =
    for {
        s <- socket
        _ <- Right(logger.debug("Sending bytes {}", input))
        _ <- Right(s.getOutputStream().write(input.toArray))
        _ <- Right(s.getOutputStream().flush())
        _ <- Right(logger.debug("Listening for bytes.."))
        first <- Right(s.getInputStream().read()) map (_.toByte)
        _ <- Right(logger.debug("First byte: {}", first))
        res <- bufferResponse(first, s.getInputStream())
        _ <- Right(logger.debug("Here some bytes {}", res))
    } yield res.toSeq


  def bufferResponse(first: Byte, input: InputStream): Either[Throwable, Array[Byte]] = {
    Identifiers.define(first) match
      case s: Simple => Right(consumeStream(input))
      case _ => Left(Throwable("Not implemented yet in tcp client"))
    
  }

  def consumeStream(input: InputStream, accumulator: Array[Byte] = Array.emptyByteArray): Array[Byte] = {
    input.read().toByte match
      case -1 => accumulator
      case R => accumulator
      case b: Byte => logger.debug("Read: {}", b); consumeStream(input, accumulator :+ b)
  }

}
