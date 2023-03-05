import scala.util.Try
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import Serialization.N
import Serialization.R

object Serialization {
  val R = '\r'.toByte
  val N = '\n'.toByte
  val RN = Seq(R, N)
  val RNs: String = new String(RN.toArray, StandardCharsets.UTF_8)

  def parseString[E >: Type[?]](
      input: String,
      charset: Charset = StandardCharsets.UTF_8
  ): Either[Throwable, E] =
    Serialization.parse(input.getBytes().toSeq, charset)

  def parse[E >: Type[?]](
      input: Seq[Byte],
      charset: Charset = StandardCharsets.UTF_8
  ): Either[Throwable, E] =
    TypeSerialization()
      .parse(input, charset)
      .map(e => (input.headOption, e))
      .flatMap(e =>
        e._1 match
          case Identifiers.blob => Right(BlobString(e._2))
          case _                => Left(Throwable("Type not found"))
      )

}

case class TypeSerialization() extends Serialization {}

trait Serialization {
  def parse(
      input: Seq[Byte],
      charset: Charset = StandardCharsets.UTF_8
  ): Either[Throwable, Seq[String]] =
    Try {
      aggregate(input.toList)
        .map(_.toArray)
        .map(e => String(e, charset))
    }.toEither

  def aggregate(
      input: List[Byte],
      accumulator: Seq[Seq[Byte]] = Seq.empty,
      element: Seq[Byte] = Seq.empty
  ): Seq[Seq[Byte]] = input match
    case Nil            => accumulator :+ element
    case R :: N :: Nil  => aggregate(Nil, accumulator, element)
    case R :: N :: tail => aggregate(tail, accumulator :+ element, Seq.empty)
    case head :: tail   => aggregate(tail, accumulator, element :+ head)
}
