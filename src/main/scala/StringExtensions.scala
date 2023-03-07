import java.nio.charset.StandardCharsets
extension (i: String)
  def toSeqOfBytes: Seq[Byte] = i.getBytes().toSeq


extension (i: Seq[Byte])
  def bytesToString: String = String(i.toArray, StandardCharsets.UTF_8)
  def bytesToInt: Int = bytesToString.toInt // TODO - optimize it