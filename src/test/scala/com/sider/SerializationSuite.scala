package com.sider

import com.sider.Resp3Serialization.RN
import com.sider.Resp3Serialization.RNs

import com.sider.Resp3Serialization
class SerializationSuite extends munit.FunSuite {
  test("TypeSerialization") {
    val head = "Hello".toStream
    val tail = "World".toStream
    val result = Resp3Serialization.aggregate(head #::: RN #::: tail)

    assertEquals(Resp3Serialization.aggregate(head), Seq(head))
    assertEquals(Resp3Serialization.aggregate(head :++ RN), Seq(head))
    assertEquals(Resp3Serialization.aggregate(head :++ RN :++ tail), Seq(head, tail))

    assertEquals(Resp3Serialization.parse(head), Right(Seq("Hello")))
    assertEquals(Resp3Serialization.parse(head :++ RN), Right(Seq("Hello")))
    assertEquals(
      Resp3Serialization.parse(head :++ RN :++ tail),
      Right(Seq("Hello", "World"))
    )
    assertEquals(
      Resp3Serialization.parse(s"Hello${RNs}World".toSeqOfBytes),
      Right(Seq("Hello", "World"))
    )
    assertEquals(
      Resp3Serialization.parse(s"$$11${RNs}helloworld${RNs}".toSeqOfBytes),
      Right(Seq("$11", "helloworld"))
    )

    assertEquals(
      Resp3Serialization.takeFirstElement(
        s"$$11${RNs}helloworld${RNs}".toStream
      ),
      "$11".toSeqOfBytes
    )
    assertEquals(
      Resp3Serialization.skip(s"$$11${RNs}helloworld${RNs}".toStream).toArray.bytesToString,
      s"helloworld${RNs}"
    )

  }
}
