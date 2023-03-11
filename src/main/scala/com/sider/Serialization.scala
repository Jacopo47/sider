package com.sider


import scala.util.Try
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import com.sider.Identifiers.define



object Serialization {

  val R = '\r'.toByte
  val N = '\n'.toByte
  val RN = Seq(R, N)
  val RNs: String = new String(RN.toArray, StandardCharsets.UTF_8)

  def readString[E >: Type[?]](
      input: String,
      charset: Charset = StandardCharsets.UTF_8
  ): Either[Throwable, E] =
    read(input.toSeqOfBytes, charset)

  def read[E >: Type[?]](
      input: Seq[Byte],
      charset: Charset = StandardCharsets.UTF_8
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
      aggregate(input)
        .map(_.toArray)
        .map(e => String(e, charset))
    }.toEither

  def aggregate(
      input: Seq[Byte],
      accumulator: Seq[Seq[Byte]] = Seq.empty,
      element: Seq[Byte] = Seq.empty
  ): Seq[Seq[Byte]] = input.toList match
    case Nil            => accumulator :+ element
    case R :: N :: Nil  => aggregate(Nil, accumulator, element)
    case R :: N :: tail => aggregate(tail, accumulator :+ element, Seq.empty)
    case head :: tail   => aggregate(tail, accumulator, element :+ head)

  def takeFirstElement(
      input: Seq[Byte],
      element: Seq[Byte] = Seq.empty
  ): Seq[Byte] = input.toList match
    case Nil            => element
    case R :: N :: Nil  => takeFirstElement(Nil, element)
    case R :: N :: tail => takeFirstElement(Nil, element)
    case head :: tail   => takeFirstElement(tail, element :+ head)

  def skip(
      input: Seq[Byte],
      element: Seq[Byte] = Seq.empty
  ): Seq[Byte] = input.toList match
    case Nil            => element
    case R :: N :: Nil  => skip(Nil, Seq.empty)
    case R :: N :: tail => skip(Nil, tail)
    case head :: tail   => skip(tail, Seq.empty)
}
