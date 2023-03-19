package com.sider

import com.sider.Serialization.RN
import com.sider.Serialization.RNs

import com.sider.Serialization
class SerializationSuite extends munit.FunSuite {
  test("TypeSerialization") {
    val head = "Hello".toStream
    val tail = "World".toStream
    val result = Serialization.aggregate(head #::: RN #::: tail)

    assertEquals(Serialization.aggregate(head), Seq(head))
    assertEquals(Serialization.aggregate(head :++ RN), Seq(head))
    assertEquals(Serialization.aggregate(head :++ RN :++ tail), Seq(head, tail))

    assertEquals(Serialization.parse(head), Right(Seq("Hello")))
    assertEquals(Serialization.parse(head :++ RN), Right(Seq("Hello")))
    assertEquals(
      Serialization.parse(head :++ RN :++ tail),
      Right(Seq("Hello", "World"))
    )
    assertEquals(
      Serialization.parse(s"Hello${RNs}World".toSeqOfBytes),
      Right(Seq("Hello", "World"))
    )
    assertEquals(
      Serialization.parse(s"$$11${RNs}helloworld${RNs}".toSeqOfBytes),
      Right(Seq("$11", "helloworld"))
    )

    assertEquals(
      Serialization.takeFirstElement(
        s"$$11${RNs}helloworld${RNs}".toStream
      ),
      "$11".toSeqOfBytes
    )
    assertEquals(
      Serialization.skip(s"$$11${RNs}helloworld${RNs}".toStream).toArray.bytesToString,
      s"helloworld${RNs}"
    )

  }
}
