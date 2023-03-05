sealed trait Type[A]:
  val identifier: Option[Byte]
  val input: Seq[String]

  lazy val metadata: Seq[String] = input match
    case head +: tail => replaceIdentifier(Some(head), identifier)
      .map(Seq(_) :++ tail)
      .getOrElse(input)
  
  def value(): Either[Throwable, A]

  def replaceIdentifier(input: Option[String], identifier: Option[Byte]): Option[String] = identifier match
    case Some(i) => input.map(e => e.replace(i.toChar, 0.toChar).trim())
    case None => input
  


object Identifiers:
    val blob: Option[Byte] = Some('$'.toByte)
    val SimpleString: Option[Byte] = Some('+'.toByte)


case class ElementNotFound() extends Throwable {}

case class BlobString(input: Seq[String]) extends Type[String] {
  override val identifier: Option[Byte] = Identifiers.blob

  def length(): Either[Throwable, Int] = metadata
    .headOption
    .flatMap(_.toIntOption)
    .map(Right(_))
    .getOrElse(Left(Throwable("No length found")))


  override def value(): Either[Throwable, String] = metadata match
    case length :+ e if metadata.length == 2 => Right(e)
    case _ => Left(ElementNotFound())
  
}