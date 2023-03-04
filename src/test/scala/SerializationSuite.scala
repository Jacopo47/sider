import Serialization.RN

class SerializationSuite extends munit.FunSuite {
  test("Basic test") {
    val head = "Hello".getBytes().toList
    val tail = "World".getBytes().toList
    val result = Serialization.aggregate(head :++ RN :++ tail)

    assertEquals(Serialization.aggregate(head), Seq(head))
    assertEquals(Serialization.aggregate(head :++ RN), Seq(head))
    assertEquals(Serialization.aggregate(head :++ RN :++ tail), Seq(head, tail))

    assertEquals(Serialization.parse(head), Right(Seq("Hello")))
    assertEquals(Serialization.parse(head :++ RN), Right(Seq("Hello")))
    assertEquals(Serialization.parse(head :++ RN :++ tail), Right(Seq("Hello", "World")))

  }
}
