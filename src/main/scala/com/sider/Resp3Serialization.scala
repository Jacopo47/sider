package com.sider

import scala.util.Try
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import com.sider.Identifiers.define
import org.slf4j.LoggerFactory
import org.slf4j.Logger
import scala.collection.LazyZip2

import com.sider.{Aggregate, Simple, Complex, Identifiers, Type}
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

  def takeFirstElement(
      input: LazyList[Byte],
      element: Seq[Byte] = Seq.empty
  ): Seq[Byte] =
    input match
      case LazyList()      => element
      case LazyList(R, _*) => element
      case head #:: tail   => takeFirstElement(tail, element :+ head)

  def skip(
      input: LazyList[Byte]
  ): LazyList[Byte] =
    input.dropWhile(!R.equals(_)).dropWhile(!N.equals(_)).drop(1)

  def skip(
      input: Seq[Byte],
      element: Seq[Byte] = Seq.empty
  ): Seq[Byte] = input.toList match
    case Nil            => element
    case R :: N :: Nil  => skip(Nil, Seq.empty)
    case R :: N :: tail => skip(Nil, tail)
    case head :: tail   => skip(tail, Seq.empty)

  def toCommand(input: String): Array[Byte] =
    val elements = input.split(" ")
    val bytes: Array[Byte] = elements
      .map(_.getBytes())
      .flatMap(e =>
        Array(
          Identifiers.BlobString.get
        ) ++ e.length.getBytes ++ RNa ++ e ++ RNa
      )

    Array(
      Identifiers.Array.get
    ) ++ elements.length.getBytes ++ RNa ++ bytes ++ RNa
}
