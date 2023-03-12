package com.sider

import com.sider.Serialization
import com.sider.{bytesToString, bytesToInt}
import java.nio.charset.StandardCharsets
import com.sider.Serialization.RN
import com.sider.Serialization.R
import com.sider.Serialization.N
import scala.collection.Map

object Identifiers {
  val BlobString = Some('$'.toByte)
  val BlobError = Some('!'.toByte)
  val VerbatimString = Some('='.toByte)

  val Array = Some('*'.toByte)
  val Map = Some('%'.toByte)
  val Set = Some('~'.toByte)
  val Attribute = Some('|'.toByte)

  val SimpleString = Some('+'.toByte)
  val SimpleError = Some('-'.toByte)
  val Number = Some(':'.toByte)
  val Null = Some('_'.toByte)
  val Double = Some(','.toByte)
  val Boolean = Some('#'.toByte)
  val BigNumber = Some('('.toByte)

  def define(input: Byte): Definition = Some(input) match
    case BlobString | BlobError | VerbatimString => Complex(Some(input))
    case Array | Map | Set | Attribute           => Aggregate(Some(input))
    case _                                       => Simple(Some(input))
}

case class ElementNotFound() extends Throwable {}
case class MissingTypeMapping() extends Throwable {}
case class AggregateTypeNotParsable() extends Throwable {}

sealed trait Type[A]:
  val identifier: Option[Byte]

  /* Type's bytes that comes from the outside.
      It expect to be passed without the initial byte but handles \r\n at the end */
  val raw: Seq[Byte]

  lazy val length: Either[Throwable, Int] = bytes.map(_.size)
  lazy val value: Either[Throwable, A]
  lazy val bytes: Either[Throwable, Seq[Byte]]

sealed trait SimpleType[A] extends Type[A] {
  lazy val bytes: Either[Throwable, Seq[Byte]] = Right(Serialization)
    .map(_.takeFirstElement(raw))
    .filterOrElse(_.nonEmpty, Throwable("Unable to define length"))
    .map(identifier.get +: _ :++ RN)
}

sealed trait ComplexType extends Type[String] {
  lazy val bytes: Either[Throwable, Seq[Byte]] = valueLength
    .map(len => raw.take(len.size + RN.size + len.bytesToInt + RN.size))
    .map(identifier.get +: _)

  lazy val valueLength: Either[Throwable, Seq[Byte]] = Right(raw)
    .map(Serialization.takeFirstElement(_))
    .filterOrElse(_.nonEmpty, Throwable("Unable to define length"))

  override lazy val value: Either[Throwable, String] = valueLength
    .map(bytesToInt)
    .map(Serialization.skip(raw).take(_))
    .map(bytesToString)
}

sealed trait AggregateType[A] extends Type[A] {

  override lazy val bytes: Either[Throwable, Seq[Byte]] = types
    .getOrElse(Seq.empty)
    .map(_.bytes)
    .partitionMap(identity) match {
    case (Nil, ok) => Right(ok flatMap (_.toSeq))
    case (ko, _)   => Left(AggregateTypeNotParsable())
  } map (identifier.get +: valueLength.getOrElse(Seq.empty) :++ RN :++ _)

  lazy val nOfElements: Either[Throwable, Int] = valueLength
    .map(bytesToInt)

  lazy val valueLength: Either[Throwable, Seq[Byte]] = Right(raw)
    .map(Serialization.takeFirstElement(_))
    .filterOrElse(_.nonEmpty, Throwable("Unable to define length"))

  lazy val types: Either[Throwable, Seq[Type[?]]] = nOfElements flatMap {
    greedyIterator(Serialization.skip(raw), Right(Seq.empty), _)
  }

  def greedyIterator(
      i: Seq[Byte],
      accumulator: Either[Throwable, Seq[Type[?]]],
      l: Int
  ): Either[Throwable, Seq[Type[?]]] = l match {
    case 0 => accumulator
    case _ => {
      val app = Serialization.read(i)
      app match
        case Left(_) =>
          greedyIterator(Seq.empty, Left(AggregateTypeNotParsable()), 0)
        case Right(value) =>
          greedyIterator(
            i.drop(value.length.getOrElse(0)),
            accumulator.map(_ :+ value),
            l - 1
          )
    }
  }

}

sealed trait Definition {
  val identifier: Option[Byte]
  def map(input: Seq[Byte]): Either[Throwable, Type[?]]
}

case class Simple(identifier: Option[Byte]) extends Definition {
  override def map(input: Seq[Byte]): Either[Throwable, Type[?]] =
    identifier match
      case Identifiers.SimpleString => Right(SimpleString(input))
      case Identifiers.SimpleError  => Right(SimpleError(input))
      case Identifiers.Number       => Right(Number(input))
      case Identifiers.Null         => Right(Null(input))
      case Identifiers.Double       => Right(Double(input))
      case Identifiers.Boolean      => Right(Boolean(input))
      case Identifiers.BigNumber    => Right(BigNumber(input))
      case _                        => Left(MissingTypeMapping())

}
case class Complex(identifier: Option[Byte]) extends Definition {

  override def map(input: Seq[Byte]): Either[Throwable, Type[?]] =
    identifier match {
      case Identifiers.BlobString     => Right(BlobString(input))
      case Identifiers.BlobError      => Right(BlobError(input))
      case Identifiers.VerbatimString => Right(VerbatimString(input))
      case _                          => Left(MissingTypeMapping())
    }
}

case class Aggregate(identifier: Option[Byte]) extends Definition {

  val app = Identifiers.Attribute

  override def map(input: Seq[Byte]): Either[Throwable, Type[?]] =
    identifier match {
      case Identifiers.Array     => Right(Array(input))
      case Identifiers.Map       => Right(Map(input))
      case Identifiers.Set       => Right(Set(input))
      case Identifiers.Attribute => Right(Attribute(input))
      case _                     => Left(MissingTypeMapping())
    }

}

case class Array(raw: Seq[Byte]) extends AggregateType[Seq[Any]] {

  override val identifier: Option[Byte] = Identifiers.Array

  /** This method is a bit optimistic because is filtering await all failed
    * types without any kind of evidence
    */
  override lazy val value: Either[Throwable, Seq[Any]] = types map {
    _.map(_.value.toOption).filter(_.isDefined).map(_.get)
  }

}

case class Set(raw: Seq[Byte])
    extends AggregateType[scala.collection.immutable.Set[Any]] {

  override val identifier: Option[Byte] = Identifiers.Array

  /** This method is a bit optimistic because is filtering await all failed
    * types without any kind of evidence
    */
  override lazy val value
      : Either[Throwable, scala.collection.immutable.Set[Any]] = types map {
    _.map(_.value.toOption)
      .filter(_.isDefined)
      .map(_.get)
      .toSet
  }

}

case class Map(raw: Seq[Byte])
    extends AggregateType[scala.collection.immutable.Map[Any, Any]] {

  override val identifier: Option[Byte] = Identifiers.Map

  override lazy val nOfElements: Either[Throwable, Int] =
    valueLength.map(bytesToInt).map(_ * 2)

  /** This method is a bit optimistic because is filtering await all failed
    * types without any kind of evidence
    */
  override lazy val value
      : Either[Throwable, scala.collection.immutable.Map[Any, Any]] =
    types map {
      _.map(_.value.toOption)
        .filter(_.isDefined)
        .map(_.get)
        .grouped(2)
        .map(e => e.head -> e.last)
        .toMap
    }

}

case class Attribute(raw: Seq[Byte]) extends AggregateType[Any] {

  override val identifier: Option[Byte] = Identifiers.Map

  override lazy val nOfElements: Either[Throwable, Int] =
    valueLength.map(bytesToInt).map(_ * 2 + 1)

  /** This method is a bit optimistic because is filtering await all failed
    * types without any kind of evidence
    */
  override lazy val value: Either[Throwable, Any] =
    types map {
      _.lastOption
        .map(_.value.toOption)
        .filter(_.isDefined)
        .map(_.get)
        .getOrElse(Left(AggregateTypeNotParsable()))
    }

  lazy val attributes: Either[Throwable, scala.collection.immutable.Map[Any, Any]] =
    types map {
      _.dropRight(1) // Drop last that it's the value for this type
        .map(_.value.toOption)
        .filter(_.isDefined)
        .map(_.get)
        .grouped(2)
        .map(e => e.head -> e.last)
        .toMap
    }

}

case class BlobString(raw: Seq[Byte]) extends ComplexType {
  override val identifier: Option[Byte] = Identifiers.BlobString
}

case class BlobError(raw: Seq[Byte]) extends ComplexType {
  override val identifier: Option[Byte] = Identifiers.BlobError
}

case class VerbatimString(raw: Seq[Byte]) extends ComplexType {
  override val identifier: Option[Byte] = Identifiers.VerbatimString
}

case class SimpleString(raw: Seq[Byte]) extends SimpleType[String] {
  override val identifier: Option[Byte] = Identifiers.SimpleString

  override lazy val value: Either[Throwable, String] =
    Right(Serialization)
      .map(_.takeFirstElement(raw.toList))
      .map(bytesToString)

}

case class SimpleError(raw: Seq[Byte]) extends SimpleType[String] {
  override val identifier: Option[Byte] = Identifiers.SimpleError

  override lazy val value: Either[Throwable, String] =
    Right(Serialization)
      .map(_.takeFirstElement(raw.toList))
      .map(bytesToString)
}

case class Number(raw: Seq[Byte]) extends SimpleType[Long] {
  override val identifier: Option[Byte] = Identifiers.Number

  override lazy val value: Either[Throwable, Long] =
    Right(Serialization)
      .map(_.takeFirstElement(raw.toList))
      .map(bytesToLong)
}

case class Null(raw: Seq[Byte]) extends SimpleType[String] {
  override val identifier: Option[Byte] = Identifiers.Number

  override lazy val value: Either[Throwable, String] = Right("Nil")
}

case class Double(raw: Seq[Byte]) extends SimpleType[scala.Double] {
  override val identifier: Option[Byte] = Identifiers.Double

  override lazy val value: Either[Throwable, scala.Double] =
    Right(Serialization)
      .map(_.takeFirstElement(raw.toList))
      .map(bytesToDouble)
}

object Boolean:
  val T = Some('t'.toByte)
  val F = Some('f'.toByte)

case class Boolean(raw: Seq[Byte]) extends SimpleType[scala.Boolean] {
  override val identifier: Option[Byte] = Identifiers.Boolean

  override lazy val value: Either[Throwable, scala.Boolean] =
    Right(Serialization)
      .map(_.takeFirstElement(raw.toList))
      .map(_.headOption)
      .flatMap {
        case Boolean.T => Right(true)
        case Boolean.F => Right(false)
        case e         => Left(Throwable(s"Unable to parse $e as boolean"))
      }

}

case class BigNumber(raw: Seq[Byte]) extends SimpleType[scala.BigInt] {
  override val identifier: Option[Byte] = Identifiers.BigNumber

  override lazy val value: Either[Throwable, scala.BigInt] =
    Right(Serialization)
      .map(_.takeFirstElement(raw.toList))
      .map(bytesToBigInt)
}
