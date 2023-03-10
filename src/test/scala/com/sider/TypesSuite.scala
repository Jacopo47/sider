package com.sider

import com.sider.Serialization
import com.sider.{BlobString, SimpleString, SimpleError, Number, Null, Double}
import Serialization.RN
import Serialization.RNs

class TypesSuite extends munit.FunSuite {

  test("Blob String") {
    val blob = Serialization.readString(s"$$10${RNs}helloworld${RNs}") match
      case e: Either[Throwable, BlobString] => e
      case _ => Left(Throwable("Unexpected type"))

    assert(blob.isRight)
    assertEquals(blob.flatMap(e => e.value), Right("helloworld"))
    assertEquals(blob.flatMap(e => e.length()), Right(10))

    // BlobString must support strings with \r\n
    val blobWithRn =
      Serialization.readString(s"$$12${RNs}hello${RNs}world${RNs}") match
        case e: Either[Throwable, BlobString] => e
        case _ => Left(Throwable("Unexpected type"))

    assert(blobWithRn.isRight)
    assertEquals(blobWithRn.flatMap(e => e.value), Right(s"hello${RNs}world"))
    assertEquals(blobWithRn.flatMap(e => e.length()), Right(12))
  }

  test("Simple String") {
    val test = Serialization.readString(s"+hello world${RNs}") match
      case e: Either[Throwable, SimpleString] => e
      case _ => Left(Throwable("Unexpected type"))

    assert(test.isRight)
    assertEquals(test.flatMap(e => e.value), Right("hello world"))
  }

  test("Simple Error") {
    val test = Serialization.readString(
      s"-ERR this is the error description${RNs}"
    ) match
      case e: Either[Throwable, SimpleError] => e
      case _ => Left(Throwable("Unexpected type"))

    assert(test.isRight)
    assertEquals(
      test.flatMap(e => e.value),
      Right("ERR this is the error description")
    )
  }

  test("Number") {
    val test = Serialization.readString(s":1234${RNs}") match
      case e: Either[Throwable, Number] => e
      case _                            => Left(Throwable("Unexpected type"))

    assert(test.isRight)
    assertEquals(test.flatMap(e => e.value), Right(1234))
  }
  test("Null") {
    val test = Serialization.readString(s"_${RNs}") match
      case e: Either[Throwable, Null] => e
      case _                          => Left(Throwable("Unexpected type"))

    assert(test.isRight)
    assertEquals(test.flatMap(e => e.value), Right("Nil"))
  }

  test("Double") {
    val test = Serialization.readString(s",1.23${RNs}") match
      case e: Either[Throwable, Double] => e
      case _                          => Left(Throwable("Unexpected type"))

    assert(test.isRight)
    assertEquals(test.flatMap(e => e.value), Right(1.23))

    assertEquals(Serialization.readString(s",-inf${RNs}").flatMap(_.value), Right(scala.Double.MinValue))
    assertEquals(Serialization.readString(s",inf${RNs}").flatMap(_.value), Right(scala.Double.MaxValue))

  }

    test("Boolean") {
    val test = Serialization.readString(s"#t${RNs}") match
      case e: Either[Throwable, Boolean] => e
      case _                          => Left(Throwable("Unexpected type"))

    assert(test.isRight)
    assertEquals(test.flatMap(e => e.value), Right(true))

    assertEquals(Serialization.readString(s"#f${RNs}").flatMap(_.value), Right(false))
    assert(Serialization.readString(s"#wrong${RNs}").flatMap(_.value).isLeft)

  }
}
