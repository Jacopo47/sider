import scala.util.Try
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

object Serialization {
  val R = '\r'.toByte
  val N = '\n'.toByte
  val RN = Seq(R, N)

  def parse(
      input: Seq[Byte],
      charset: Charset = StandardCharsets.UTF_8
  ): Either[Throwable, Seq[String]] =
    Try {
      aggregate(input.toList)
        .map(_.toArray)
        .map(e => new String(e, charset))
    }.toEither

  def aggregate(input: List[Byte], accumulator: Seq[Seq[Byte]] = Seq.empty, element: Seq[Byte] = Seq.empty): Seq[Seq[Byte]] = input match
    case Nil => accumulator :+ element
    case R :: N :: Nil => aggregate(Nil, accumulator, element)
    case R :: N :: tail => aggregate(tail, accumulator :+ element, Seq.empty)
    case head :: tail => aggregate(tail, accumulator, element :+ head)
}
