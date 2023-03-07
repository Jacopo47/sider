
import java.nio.charset.StandardCharsets
sealed trait Type[A]:
  val identifier: Option[Byte]
  val raw: Seq[Byte]

  lazy val value: Either[Throwable, A]

  def replaceIdentifier(input: Option[String], identifier: Option[Byte]): Option[String] = identifier match
    case Some(i) => input.map(e => e.replace(i.toChar, 0.toChar).trim())
    case None => input
  

sealed trait Definition {
  val identifier: Option[Byte]
  def map(input: Seq[Byte]): Type[?]
}

case class Simple(identifier: Option[Byte]) extends Definition {
  override def map(input: Seq[Byte]): Type[?] = identifier match
    case Identifiers.SimpleString => SimpleString(input)
}
case class Complex(identifier: Option[Byte]) extends Definition {

  override def map(input: Seq[Byte]): Type[?] = ???

}
case class Aggregate(identifier: Option[Byte]) extends Definition {

  override def map(input: Seq[Byte]): Type[?] = ???

}


object Identifiers:
    val BlobString = Some('$'.toByte)
    val BlobError = Some('!'.toByte)
    val VerbatimString = Some('='.toByte)

    val Array = Some('*'.toByte)
    val Map = Some('%'.toByte)
    val Set = Some('~'.toByte)

     
    val SimpleString = Some('+'.toByte)
    val SimpleError = Some('-'.toByte)
    val Number = Some(':'.toByte)
    val Null = Some('_'.toByte)
    val Double = Some(','.toByte)
    val Boolean = Some('#'.toByte)
    val BigNumber = Some('('.toByte)


    def define(input: Byte): Definition = Some(input) match
      case BlobString | BlobError | VerbatimString => Complex(Some(input))
      case Array | Map | Set => Aggregate(Some(input))
      case _ => Simple(Some(input))



case class ElementNotFound() extends Throwable {}

case class BlobString(raw: Seq[Byte]) extends Type[String] {

  override val identifier: Option[Byte] = Identifiers.BlobString

  def length(): Either[Throwable, Int] = Right(TypeSerialization())
    .map(_.takeFirstElement(raw.toList))
    .filterOrElse(_.nonEmpty, Throwable("Unable to define blob string lenght"))
    .map(bytesToInt)
  
  
  override lazy val value: Either[Throwable, String] = length()
  .map(TypeSerialization().skip(raw).take(_))
  .map(bytesToString)
}


case class SimpleString(raw: Seq[Byte]) extends Type[String] {
  override val identifier: Option[Byte] = Identifiers.SimpleString

  override lazy val value: Either[Throwable, String] = Right(TypeSerialization())
  .map(_.takeFirstElement(raw.toList))
  .map(bytesToString)
  
}