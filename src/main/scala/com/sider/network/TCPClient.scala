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

  def gossip(input: Array[Byte]): Either[Throwable, Array[Byte]] =
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
    } yield res

  def gossip(input: String): Either[Throwable, Array[Byte]] =
      this.gossip(Serialization.toResp3Command(input))



  def bufferResponse(first: Byte, input: InputStream): Either[Throwable, Array[Byte]] = {
    Identifiers.define(first) match
      case s: Simple => Right(bufferSingleElement(input, Array(first)))
      case c: Complex =>
          val metadata = bufferSingleElement(input, Array(first))
          val len: Long = metadata
            .drop(1)
            .dropRight(2)
            .bytesToLong

          val blob = bufferBlob(input, Array.emptyByteArray, len)

          Right(metadata ++ blob)

          

      case _ => Left(Throwable("Not implemented yet in tcp client"))
    
  }

  @tailrec
  private def bufferSingleElement(input: InputStream, accumulator: Array[Byte] = Array.emptyByteArray): Array[Byte] = {
    val next = input.read()
    
    next.toByte match
      case TCPClient.endOfStreamByte => accumulator
      case R => accumulator :+ R :+ input.read().toByte
      case b: Byte => logger.debug("Read: {}", b); bufferSingleElement(input, accumulator :+ b)
  }

  @tailrec
  private def bufferBlob(input: InputStream, accumulator: Array[Byte] = Array.emptyByteArray, numberOfElements: Long): Array[Byte] = {
    numberOfElements match
      case 0 => accumulator ++ RNa
      case _ => bufferBlob(input, accumulator :+ input.read().toByte, numberOfElements - 1)
  }

}

object TCPClient:
  val endOfStreamByte = -1.toByte
