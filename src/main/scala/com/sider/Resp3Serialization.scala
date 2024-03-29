package com.sider

import scala.util.Try
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import com.sider.Identifiers.define
import org.slf4j.LoggerFactory
import org.slf4j.Logger
import scala.collection.LazyZip2

import com.sider.{Aggregate, Complex, Identifiers, Simple, Type}
object Resp3Serialization {

  val logger: Logger = LoggerFactory.getLogger("Serialization")

  val R = '\r'.toByte
  val N = '\n'.toByte
  val RN = LazyList(R, N)
  val RNa = RN.toArray
  val RNs: String = new String(RNa, StandardCharsets.UTF_8)

  def readString[E >: Type[?]](
      input: String
  ): Either[Throwable, E] =
    read(input.toStream)

  def read[E >: Type[?]](
      input: LazyList[Byte]
  ): Either[Throwable, E] =
    input match
      case head +: tail =>
        define(head) match
          case e: Simple => e.map(tail)

          case c: Complex => c.map(tail)

          case a: Aggregate => a.map(tail)
      case _ => Left(Throwable("Not implemented yet"))

  def parse(
      input: Seq[Byte],
      charset: Charset = StandardCharsets.UTF_8
  ): Either[Throwable, Seq[String]] =
    Try {
      aggregate(input.to(LazyList))
        .map(_.toArray)
        .map(e => String(e, charset))
    }.toEither

  @tailrec
  def aggregate(
      input: LazyList[Byte],
      accumulator: Seq[Seq[Byte]] = Seq.empty,
      element: Seq[Byte] = Seq.empty
  ): Seq[Seq[Byte]] = input match
    case LazyList() => accumulator :+ element
    case R #:: N #:: LazyList() =>
      aggregate(LazyList.empty, accumulator, element)
    case R #:: N #:: tail => aggregate(tail, accumulator :+ element, Seq.empty)
    case head #:: tail    => aggregate(tail, accumulator, element :+ head)

  @tailrec
  def takeFirstElement(
      input: LazyList[Byte],
      element: Seq[Byte] = Seq.empty
  ): Seq[Byte] =
    input match
      case LazyList()      => element
      case LazyList(R, _*) => element
      case head #:: tail   => takeFirstElement(tail, element :+ head)

  /**
   * Skips until it found the next element. </p> Element are split by Resp3
   * terminator (CRLF)
   */
  def skip(
      input: LazyList[Byte]
  ): LazyList[Byte] =
    input.dropWhile(!R.equals(_)).dropWhile(!N.equals(_)).drop(1)

  /**
   * Skips until it found the next element. </p> Element are split by Resp3
   * terminator (CRLF)
   */
  @tailrec
  def skip(
      input: Seq[Byte],
      element: Seq[Byte] = Seq.empty
  ): Seq[Byte] = input.toList match
    case Nil            => element
    case R :: N :: Nil  => skip(Nil, Seq.empty)
    case R :: N :: tail => skip(Nil, tail)
    case head :: tail   => skip(tail, Seq.empty)

  /**
   * Transforms the input string in a Resp3 Array. </p> It splits element by
   * whitespace. If a single element is passed. Treats the input already
   * splitted if more than 1 elements is passed.
   *
   * <b>IMPORTANT:</b> Prefer passing arguments by Array of elements since the
   * split logic is dummy right now and some scenarios (like whitespace in a
   * string) are not supported. "The dummy version" could be useful for testing
   * purposes right now. And probably will be removed in future.
   */
  def toCommand(input: String*): Array[Byte] =
    logger.debug("Preparing command: {}", input)
    val elements =
      if input.size == 1 then input(0).split(" ") else input.toArray

    val bytes: Array[Array[Byte]] = elements
      .filter(_ != null)
      .map(_.getBytes())

    this.toCommandFromBytes(bytes : _*)

  def toCommandFromBytes(input: Array[Byte]*): Array[Byte] =
    val bytes = input.flatMap(e =>
        Array(
          Identifiers.BlobString.get
        ) ++ e.length.getBytes ++ RNa ++ e ++ RNa
      )
    
    Array(
      Identifiers.Array.get
    ) ++ input.length.getBytes ++ RNa ++ bytes ++ RNa
}
