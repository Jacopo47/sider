package com.sider

import com.sider.TypeSerialization
import com.sider.{bytesToString, bytesToInt}
import java.nio.charset.StandardCharsets

sealed trait Type[A]:
  val identifier: Option[Byte]
  val raw: Seq[Byte]

  lazy val value: Either[Throwable, A]

  def replaceIdentifier(
      input: Option[String],
      identifier: Option[Byte]
  ): Option[String] = identifier match
    case Some(i) => input.map(e => e.replace(i.toChar, 0.toChar).trim())
    case None    => input

sealed trait Definition {
  val identifier: Option[Byte]
  def map(input: Seq[Byte]): Either[Throwable, Type[?]]
}

case class MissingTypeMapping() extends Throwable {}

case class Simple(identifier: Option[Byte]) extends Definition {
  override def map(input: Seq[Byte]): Either[Throwable, Type[?]] =
    identifier match
      case Identifiers.SimpleString => Right(SimpleString(input))
      case Identifiers.SimpleError  => Right(SimpleError(input))
      case Identifiers.Number       => Right(Number(input))
      case Identifiers.Null         => Right(Null(input))
      case Identifiers.Double       => Right(Double(input))
      case Identifiers.Boolean      => Right(Boolean(input))
      case Identifiers.BigNumber    => ???
      case _                        => Left(MissingTypeMapping())

}
case class Complex(identifier: Option[Byte]) extends Definition {

  override def map(input: Seq[Byte]): Either[Throwable, Type[?]] = ???

}
case class Aggregate(identifier: Option[Byte]) extends Definition {

  override def map(input: Seq[Byte]): Either[Throwable, Type[?]] = ???

}

object Identifiers:
  val BlobString = Some('$'.toByte)
  val BlobError = Some('!'.toByte)
  val VerbatimString = Some('='.toByte)

  val Array = Some('*'.toByte)
  val Map = Some('%'.toByte)
  val Set = Some('~'.toByte)

  val SimpleString = Some('+'.toByte)
  val SimpleError = Some('-'.toByte)
  val Number = Some(':'.toByte)
  val Null = Some('_'.toByte)
  val Double = Some(','.toByte)
  val Boolean = Some('#'.toByte)
  val BigNumber = Some('('.toByte)

  def define(input: Byte): Definition = Some(input) match
    case BlobString | BlobError | VerbatimString => Complex(Some(input))
    case Array | Map | Set                       => Aggregate(Some(input))
    case _                                       => Simple(Some(input))

case class ElementNotFound() extends Throwable {}

case class BlobString(raw: Seq[Byte]) extends Type[String] {

  override val identifier: Option[Byte] = Identifiers.BlobString

  def length(): Either[Throwable, Int] = Right(TypeSerialization())
    .map(_.takeFirstElement(raw.toList))
    .filterOrElse(_.nonEmpty, Throwable("Unable to define blob string length"))
    .map(bytesToInt)

  override lazy val value: Either[Throwable, String] = length()
    .map(TypeSerialization().skip(raw).take(_))
    .map(bytesToString)
}

case class SimpleString(raw: Seq[Byte]) extends Type[String] {
  override val identifier: Option[Byte] = Identifiers.SimpleString

  override lazy val value: Either[Throwable, String] =
    Right(TypeSerialization())
      .map(_.takeFirstElement(raw.toList))
      .map(bytesToString)

}

case class SimpleError(raw: Seq[Byte]) extends Type[String] {
  override val identifier: Option[Byte] = Identifiers.SimpleError

  override lazy val value: Either[Throwable, String] =
    Right(TypeSerialization())
      .map(_.takeFirstElement(raw.toList))
      .map(bytesToString)
}

case class Number(raw: Seq[Byte]) extends Type[Int] {
  override val identifier: Option[Byte] = Identifiers.Number

  override lazy val value: Either[Throwable, Int] =
    Right(TypeSerialization())
      .map(_.takeFirstElement(raw.toList))
      .map(bytesToInt)
}

case class Null(raw: Seq[Byte]) extends Type[String] {
  override val identifier: Option[Byte] = Identifiers.Number

  override lazy val value: Either[Throwable, String] = Right("Nil")
}

case class Double(raw: Seq[Byte]) extends Type[scala.Double] {
  override val identifier: Option[Byte] = Identifiers.Double

  override lazy val value: Either[Throwable, scala.Double] =
    Right(TypeSerialization())
      .map(_.takeFirstElement(raw.toList))
      .map(bytesToDouble)
}


case class Boolean(raw: Seq[Byte]) extends Type[scala.Boolean] {
  override val identifier: Option[Byte] = Identifiers.Boolean

  // TODO - move to a companion object
  val T = Seq('t'.toByte)
  val F = Seq('f'.toByte)

  override lazy val value: Either[Throwable, scala.Boolean] =
    Right(TypeSerialization())
      .map(_.takeFirstElement(raw.toList))
      .flatMap( e => e match {
        case T => Right(true)
        case F => Right(false)
        case _ => Left(Throwable(s"Unable to parse ${e} as boolean"))
      })
      
}