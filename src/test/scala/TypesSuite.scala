import Serialization.RN
import Serialization.RNs

class TypesSuite extends munit.FunSuite {

  test("Blob String") {
    val blob = Serialization.parseString(s"$$10${RNs}helloworld${RNs}") match
      case e: Either[Throwable, BlobString] => e
      case _ => Left(Throwable("Unexpected type"))

    assert(blob.isRight)
    assertEquals(blob.flatMap(e => e.value), Right("helloworld"))
    assertEquals(blob.flatMap(e => e.length()), Right(10))

    // BlobString must support strings with \r\n
    val blobWithRn =
      Serialization.parseString(s"$$12${RNs}hello${RNs}world${RNs}") match
        case e: Either[Throwable, BlobString] => e
        case _ => Left(Throwable("Unexpected type"))

    assert(blobWithRn.isRight)
    assertEquals(blobWithRn.flatMap(e => e.value), Right(s"hello${RNs}world"))
    assertEquals(blobWithRn.flatMap(e => e.length()), Right(12))
  }

  test("Simple String") {
    val test = Serialization.parseString(s"+hello world${RNs}") match
      case e: Either[Throwable, SimpleString] => e
      case _ => Left(Throwable("Unexpected type"))

    assert(test.isRight)
    assertEquals(test.flatMap(e => e.value), Right("hello world"))
  }
}
