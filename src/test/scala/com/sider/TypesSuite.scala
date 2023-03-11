package com.sider

import com.sider.Serialization
import com.sider.{BlobString, SimpleString, SimpleError, Number, Null, Double}
import Serialization.RN
import Serialization.RNs

class TypesSuite extends munit.FunSuite {

  test("Blob String") {
    val blob = Serialization.readString(s"$$10${RNs}helloworld$RNs") match
      case e: Either[Throwable, BlobString] => e
      case _ => Left(Throwable("Unexpected type"))

    assert(blob.isRight)
    assertEquals(blob.flatMap(e => e.value), Right("helloworld"))
    assertEquals(blob.flatMap(e => e.valueLength).map(_.bytesToInt), Right(10))
    assertEquals(blob.flatMap(e => e.length), Right(17))
    assertEquals(
      blob.flatMap(_.bytes).map(bytesToString),
      Right(s"$$10${RNs}helloworld$RNs")
    )

    // BlobString must support strings with \r\n
    val blobWithRn =
      Serialization.readString(s"$$12${RNs}hello${RNs}world$RNs") match
        case e: Either[Throwable, BlobString] => e
        case _ => Left(Throwable("Unexpected type"))

    assert(blobWithRn.isRight)
    assertEquals(blobWithRn.flatMap(e => e.value), Right(s"hello${RNs}world"))
    assertEquals(
      blobWithRn.flatMap(e => e.valueLength).map(_.bytesToInt),
      Right(12)
    )
  }

  test("Blob error") {
    val blob =
      Serialization.readString(s"!21${RNs}SYNTAX invalid syntax${RNs}") match
        case e: Either[Throwable, BlobError] => e
        case _ => Left(Throwable("Unexpected type"))

    assert(blob.isRight)
    assertEquals(blob.flatMap(e => e.value), Right("SYNTAX invalid syntax"))
    assertEquals(blob.flatMap(e => e.valueLength).map(_.bytesToInt), Right(21))
  }

  test("Verbatim String") {
    val blob = Serialization.readString(s"=15${RNs}txt:Some string${RNs}") match
      case e: Either[Throwable, VerbatimString] => e
      case _ => Left(Throwable("Unexpected type"))

    assert(blob.isRight)
    assertEquals(blob.flatMap(e => e.value), Right("txt:Some string"))
    assertEquals(blob.flatMap(e => e.valueLength).map(_.bytesToInt), Right(15))
  }

  test("Simple String") {
    val test = Serialization.readString(s"+hello world$RNs") match
      case e: Either[Throwable, SimpleString] => e
      case _ => Left(Throwable("Unexpected type"))

    assert(test.isRight)
    assertEquals(test.flatMap(e => e.value), Right("hello world"))
  }

  test("Simple Error") {
    val test = Serialization.readString(
      s"-ERR this is the error description$RNs"
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
    val test = Serialization.readString(s":1234$RNs") match
      case e: Either[Throwable, Number] => e
      case _                            => Left(Throwable("Unexpected type"))

    assert(test.isRight)
    assertEquals(test.flatMap(e => e.value), Right(1234L))
    assertEquals(test.flatMap(e => e.length), Right(7))
    assertEquals(
      test.flatMap(e => e.bytes).map(bytesToString),
      Right(s":1234$RNs")
    )
  }
  test("Null") {
    val test = Serialization.readString(s"_$RNs") match
      case e: Either[Throwable, Null] => e
      case _                          => Left(Throwable("Unexpected type"))

    assert(test.isRight)
    assertEquals(test.flatMap(e => e.value), Right("Nil"))
  }

  test("Double") {
    val test = Serialization.readString(s",1.23$RNs") match
      case e: Either[Throwable, Double] => e
      case _                            => Left(Throwable("Unexpected type"))

    assert(test.isRight)
    assertEquals(test.flatMap(e => e.value), Right(1.23))

    assertEquals(
      Serialization.readString(s",-inf$RNs").flatMap(_.value),
      Right(scala.Double.MinValue)
    )
    assertEquals(
      Serialization.readString(s",inf$RNs").flatMap(_.value),
      Right(scala.Double.MaxValue)
    )

  }

  test("Boolean") {
    val test = Serialization.readString(s"#t$RNs") match
      case e: Either[Throwable, Boolean] => e
      case _                             => Left(Throwable("Unexpected type"))

    assert(test.isRight)
    assertEquals(test.flatMap(e => e.value), Right(true))

    assertEquals(
      Serialization.readString(s"#f$RNs").flatMap(_.value),
      Right(false)
    )
    assert(Serialization.readString(s"#wrong$RNs").flatMap(_.value).isLeft)

  }

  test("Big Number") {
    val test = Serialization.readString(
      s"(3492890328409238509324850943850943825024385$RNs"
    ) match
      case e: Either[Throwable, BigNumber] => e
      case _                               => Left(Throwable("Unexpected type"))

    assert(test.isRight)
    assertEquals(
      test.flatMap(e => e.value),
      Right(BigInt.apply("3492890328409238509324850943850943825024385"))
    )

    assertEquals(
      Serialization.readString(s"(19$RNs").flatMap(_.value),
      Right(19)
    )

  }

  test("Array") {
    val test = Serialization.readString(s"*3$RNs:1$RNs:2$RNs:3$RNs") match
      case e: Either[Throwable, Array] => e
      case _                               => Left(Throwable("Unexpected type"))

    val a = test.flatMap(_.bytes)
    assertEquals(test.flatMap(_.length), Right(16))
    assertEquals(test.flatMap(e => e.value), Right(Seq(1, 2, 3)))
    assertEquals(Serialization.readString(s"*4$RNs:1$RNs:2$RNs:3$RNs+Helloworld").flatMap(_.value), Right(Seq(1, 2, 3, "Helloworld")))

    val blobWithEndCharInside = s"$$12${RNs}hello${RNs}world"
    assertEquals(Serialization.readString(s"*5$RNs${blobWithEndCharInside}$RNs:1$RNs:2$RNs:3$RNs+Helloworld").flatMap(_.value), Right(Seq(s"hello${RNs}world", 1, 2, 3, "Helloworld")))


    // Testing nested Arrays
    assertEquals(Serialization.readString(s"*1$RNs*3$RNs:1$RNs:2$RNs:3$RNs").flatMap(_.value), Right(Seq(Seq(1,2,3))))
    val nested = Serialization.readString(s"*2$RNs*3$RNs:1${RNs}$$5${RNs}hello$RNs:2${RNs}#f$RNs")
    assertEquals(nested.flatMap(_.length), Right(31))
    assertEquals(nested.flatMap(_.value), Right(Seq(Seq(1,"hello",2), false)))


    
    // Testing edge cases
    assertEquals(Serialization.readString(s"*0$RNs$RNs").flatMap(_.value), Right(Seq.empty))

  }
}
