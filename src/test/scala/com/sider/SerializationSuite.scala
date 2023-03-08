
import com.sider.{Serialization, TypeSerialization}
import com.sider.toSeqOfBytes
import Serialization.RN
import Serialization.RNs

class SerializationSuite extends munit.FunSuite {
  test("TypeSerialization") {
    val head = "Hello".getBytes().toList
    val tail = "World".getBytes().toList
    val result = TypeSerialization().aggregate(head :++ RN :++ tail)

    assertEquals(TypeSerialization().aggregate(head), Seq(head))
    assertEquals(TypeSerialization().aggregate(head :++ RN), Seq(head))
    assertEquals(TypeSerialization().aggregate(head :++ RN :++ tail), Seq(head, tail))

    assertEquals(TypeSerialization().parse(head), Right(Seq("Hello")))
    assertEquals(TypeSerialization().parse(head :++ RN), Right(Seq("Hello")))
    assertEquals(TypeSerialization().parse(head :++ RN :++ tail), Right(Seq("Hello", "World")))
    assertEquals(TypeSerialization().parse(s"Hello${RNs}World".toSeqOfBytes), Right(Seq("Hello", "World")))
    assertEquals(TypeSerialization().parse(s"$$11${RNs}helloworld${RNs}".toSeqOfBytes), Right(Seq("$11", "helloworld")))

    assertEquals(TypeSerialization().takeFirstElement(s"$$11${RNs}helloworld${RNs}".toSeqOfBytes.toList), "$11".toSeqOfBytes)
    assertEquals(TypeSerialization().skip(s"$$11${RNs}helloworld${RNs}".toSeqOfBytes), s"helloworld${RNs}".toSeqOfBytes)

    }
}
