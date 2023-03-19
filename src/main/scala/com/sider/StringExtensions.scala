package com.sider

import java.nio.charset.StandardCharsets
import java.lang.Number

object StringExtensions:
  val inf: Seq[Byte] = "inf".toSeqOfBytes
  val negativeInf: Seq[Byte] = "-inf".toSeqOfBytes
  val infAsArray: scala.Array[Byte] = "inf".getBytes()
  val negativeInfAsArray: scala.Array[Byte] = "-inf".getBytes()

extension (i: String) 
  def toSeqOfBytes: Seq[Byte] = i.getBytes().toSeq
  def toStream: LazyList[Byte] = i.getBytes().to(LazyList)

extension (i: Seq[Byte])
  def bytesToString: String = String(i.toArray, StandardCharsets.UTF_8)
  def bytesToInt: Int = bytesToString.toInt // TODO - optimize it
  def bytesToBigInt: BigInt = BigInt.apply(bytesToString) // TODO - optimize it

  def bytesToLong: Long = bytesToString.toLong
  def bytesToDouble: scala.Double = i match
    case StringExtensions.inf         => scala.Double.MaxValue
    case StringExtensions.negativeInf => scala.Double.MinValue
    case _                            => bytesToString.toDouble

extension (i: Array[Byte])
  def bytesToString: String = String(i, StandardCharsets.UTF_8)
  def bytesToInt: Int = bytesToString.toInt // TODO - optimize it
  def bytesToBigInt: BigInt = BigInt.apply(bytesToString) // TODO - optimize it

  def bytesToLong: Long = bytesToString.toLong
  def bytesToDouble: scala.Double = i match
    case StringExtensions.infAsArray         => scala.Double.MaxValue
    case StringExtensions.negativeInfAsArray => scala.Double.MinValue
    case _                                   => bytesToString.toDouble

extension (i: Int)
  def getBytes: Array[Byte] = i.toString().getBytes()