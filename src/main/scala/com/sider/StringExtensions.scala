package com.sider

import java.nio.charset.StandardCharsets

object StringExtensions:
  val inf: Seq[Byte] = "inf".toSeqOfBytes
  val negativeInf: Seq[Byte] = "-inf".toSeqOfBytes

extension (i: String) def toSeqOfBytes: Seq[Byte] = i.getBytes().toSeq

extension (i: Seq[Byte])
  def bytesToString: String = String(i.toArray, StandardCharsets.UTF_8)
  def bytesToInt: Int = bytesToString.toInt // TODO - optimize it
  def bytesToBigInt: BigInt = BigInt.apply(bytesToString) // TODO - optimize it
  
  def bytesToLong: Long = bytesToString.toLong
  def bytesToDouble: scala.Double = i match
    case StringExtensions.inf         => scala.Double.MaxValue
    case StringExtensions.negativeInf => scala.Double.MinValue
    case _                            => bytesToString.toDouble
