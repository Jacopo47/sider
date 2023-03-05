import Serialization.RN
import Serialization.RNs

class TypesSuite extends munit.FunSuite {

  test("Blob String") {
    val blob = Serialization.parseString(s"$$11${RNs}helloworld${RNs}") match
      case e: Either[Throwable, BlobString] => e
      case _ => Left(Throwable("Unexpected type"))

    assert(blob.isRight)
    assertEquals(blob.flatMap(e => e.value()), Right("helloworld"))
    assertEquals(blob.flatMap(e => e.length()), Right(11))
  }
}
