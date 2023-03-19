package com.sider

import com.sider.Resp3Serialization
import com.sider.{bytesToString, bytesToInt}
import java.nio.charset.StandardCharsets
import com.sider.Resp3Serialization.RN
import com.sider.Resp3Serialization.R
import com.sider.Resp3Serialization.N
import collection.Map

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
  def identifier: Option[Byte]

  /* Type's bytes that comes from the outside.
      It expect to be passed without the initial byte but handles \r\n at the end */
  val raw: LazyList[Byte]

  val value: Either[Throwable, A]
  lazy val length: Either[Throwable, Int] = bytes.map(_.size)
  lazy val bytes: Either[Throwable, Seq[Byte]]

sealed trait SimpleType[A] extends Type[A] {
  lazy val bytes: Either[Throwable, Seq[Byte]] = Right(Resp3Serialization)
    .map(_.takeFirstElement(raw))
    .filterOrElse(_.nonEmpty, Throwable("Unable to define length"))
    .map(identifier.get +: _ :++ RN)
}

sealed trait ComplexType extends Type[String] {
  lazy val bytes: Either[Throwable, Seq[Byte]] = valueLength
    .map(len => raw.take(len.size + RN.size + len.bytesToInt + RN.size))
    .map(identifier.get +: _)

  val valueLength: Either[Throwable, Seq[Byte]] = Right(raw)
    .map(Resp3Serialization.takeFirstElement(_))
    .filterOrElse(_.nonEmpty, Throwable("Unable to define length"))

  override val value: Either[Throwable, String] = bytes
    .map(Resp3Serialization.skip(_))
    .map(_.dropRight(2))
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

  val valueLength: Either[Throwable, Seq[Byte]] = Right(raw)
    .map(Resp3Serialization.takeFirstElement(_))
    .filterOrElse(_.nonEmpty, Throwable("Unable to define length"))

  lazy val types: Either[Throwable, Seq[Type[?]]] = nOfElements flatMap {
    greedyIterator(Resp3Serialization.skip(raw), Right(Seq.empty), _)
  }

  def greedyIterator(
      i: LazyList[Byte],
      accumulator: Either[Throwable, Seq[Type[?]]],
      l: Int
  ): Either[Throwable, Seq[Type[?]]] = l match {
    case 0 => accumulator
    case _ => {
      val app = Resp3Serialization.read(i)
      app match
        case Left(_) =>
          greedyIterator(LazyList.empty, Left(AggregateTypeNotParsable()), 0)
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
  def map(input: LazyList[Byte]): Either[Throwable, Type[?]]
}

case class Simple(identifier: Option[Byte]) extends Definition {
  override def map(input: LazyList[Byte]): Either[Throwable, Type[?]] =
    identifier match
      case Identifiers.SimpleString => Right(SimpleString(input))
      case Identifiers.SimpleError  => Right(SimpleError(input))
      case Identifiers.Number       => Right(Number(input))
      case Identifiers.Null         => Right(Null(input))
      case Identifiers.Double       => Right(Resp3Double(input))
      case Identifiers.Boolean      => Right(Resp3Boolean(input))
      case Identifiers.BigNumber    => Right(BigNumber(input))
      case _                        => Left(MissingTypeMapping())

}
case class Complex(identifier: Option[Byte]) extends Definition {

  override def map(input: LazyList[Byte]): Either[Throwable, Type[?]] =
    identifier match {
      case Identifiers.BlobString     => Right(BlobString(input))
      case Identifiers.BlobError      => Right(BlobError(input))
      case Identifiers.VerbatimString => Right(VerbatimString(input))
      case _                          => Left(MissingTypeMapping())
    }
}

case class Aggregate(identifier: Option[Byte]) extends Definition {
  override def map(input: LazyList[Byte]): Either[Throwable, Type[?]] =
    identifier match {
      case Identifiers.Array     => Right(Resp3Array(input))
      case Identifiers.Map       => Right(Resp3Map(input))
      case Identifiers.Set       => Right(Resp3Set(input))
      case Identifiers.Attribute => Right(Attribute(input))
      case _                     => Left(MissingTypeMapping())
    }

}

case class Resp3Array(raw: LazyList[Byte]) extends AggregateType[Seq[Any]] {

  override def identifier: Option[Byte] = Identifiers.Array

  /** This method is a bit optimistic because is filtering await all failed
    * types without any kind of evidence
    */
  override val value: Either[Throwable, Seq[Any]] = types map {
    _.map(_.value.toOption).filter(_.isDefined).map(_.get)
  }

}

case class Resp3Set(raw: LazyList[Byte])
    extends AggregateType[Set[Any]] {

  override def identifier: Option[Byte] = Identifiers.Array

  /** This method is a bit optimistic because is filtering await all failed
    * types without any kind of evidence
    */
  override val value: Either[Throwable, Set[Any]] =
    types map {
      _.map(_.value.toOption)
        .filter(_.isDefined)
        .map(_.get)
        .toSet
    }

}

case class Resp3Map(raw: LazyList[Byte])
    extends AggregateType[Map[Any, Any]] {

  override def identifier: Option[Byte] = Identifiers.Map

  override lazy val nOfElements: Either[Throwable, Int] =
    valueLength.map(bytesToInt).map(_ * 2)

  /** This method is a bit optimistic because is filtering await all failed
    * types without any kind of evidence
    */
  override val value
      : Either[Throwable, Map[Any, Any]] =
    types map {
      _.map(_.value.toOption)
        .filter(_.isDefined)
        .map(_.get)
        .grouped(2)
        .map(e => e.head -> e.last)
        .toMap
    }

}

case class Attribute(raw: LazyList[Byte]) extends AggregateType[Any] {

  override def identifier: Option[Byte] = Identifiers.Map

  override lazy val nOfElements: Either[Throwable, Int] =
    valueLength.map(bytesToInt).map(_ * 2 + 1)

  /** This method is a bit optimistic because is filtering await all failed
    * types without any kind of evidence
    */
  override val value: Either[Throwable, Any] =
    types map {
      _.lastOption
        .map(_.value.toOption)
        .filter(_.isDefined)
        .map(_.get)
        .getOrElse(Left(AggregateTypeNotParsable()))
    }

  lazy val attributes
      : Either[Throwable, Map[Any, Any]] =
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

case class BlobString(raw: LazyList[Byte]) extends ComplexType {
  override def identifier: Option[Byte] = Identifiers.BlobString
}

case class BlobError(raw: LazyList[Byte]) extends ComplexType {
  override def identifier: Option[Byte] = Identifiers.BlobError
}

case class VerbatimString(raw: LazyList[Byte]) extends ComplexType {
  override def identifier: Option[Byte] = Identifiers.VerbatimString
}

case class SimpleString(raw: LazyList[Byte]) extends SimpleType[String] {
  override def identifier: Option[Byte] = Identifiers.SimpleString

  override val value: Either[Throwable, String] =
    Right(Resp3Serialization)
      .map(_.takeFirstElement(raw))
      .map(bytesToString)

}

case class SimpleError(raw: LazyList[Byte]) extends SimpleType[String] {
  override def identifier: Option[Byte] = Identifiers.SimpleError

  override val value: Either[Throwable, String] =
    Right(Resp3Serialization)
      .map(_.takeFirstElement(raw))
      .map(bytesToString)
}

case class Number(raw: LazyList[Byte]) extends SimpleType[Long] {
  override def identifier: Option[Byte] = Identifiers.Number

  override val value: Either[Throwable, Long] =
    Right(Resp3Serialization)
      .map(_.takeFirstElement(raw))
      .map(bytesToLong)
}

case class Null(raw: LazyList[Byte]) extends SimpleType[String] {
  override def identifier: Option[Byte] = Identifiers.Number

  override val value: Either[Throwable, String] = Right("Nil")
}

case class Resp3Double(raw: LazyList[Byte]) extends SimpleType[Double] {
  override def identifier: Option[Byte] = Identifiers.Double

  override val value: Either[Throwable, Double] =
    Right(Resp3Serialization)
      .map(_.takeFirstElement(raw))
      .map(bytesToDouble)
}

object Resp3Boolean:
  val T = Some('t'.toByte)
  val F = Some('f'.toByte)

case class Resp3Boolean(raw: LazyList[Byte]) extends SimpleType[Boolean] {
  override def identifier: Option[Byte] = Identifiers.Boolean

  override val value: Either[Throwable, Boolean] =
    Right(Resp3Serialization)
      .map(_.takeFirstElement(raw))
      .map(_.headOption)
      .flatMap {
        case Resp3Boolean.T => Right(true)
        case Resp3Boolean.F => Right(false)
        case e         => Left(Throwable(s"Unable to parse $e as boolean"))
      }

}

case class BigNumber(raw: LazyList[Byte]) extends SimpleType[BigInt] {
  override def identifier: Option[Byte] = Identifiers.BigNumber

  override val value: Either[Throwable, BigInt] =
    Right(Resp3Serialization)
      .map(_.takeFirstElement(raw))
      .map(bytesToBigInt)
}
