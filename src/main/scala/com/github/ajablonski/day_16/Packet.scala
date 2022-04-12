package com.github.ajablonski.day_16

import com.github.ajablonski.Day16

sealed trait Packet(version: Long) {
  def getValue: Long
}

object Packet {
  private val literalPacketRegex = raw"([01]{3})100((?:1[01]{4})*0[01]{4})([01]*)".r
  private val operatorPacketBitLength = raw"([01]{3})([01]{3})0([01]{15})([01]*)".r
  private val operatorPacketSubpacketCountLength = raw"([01]{3})([01]{3})1([01]{11})([01]*)".r

  def parse(binary: String): Seq[Packet] = {
    import BinaryStringExtensions.fromBinary
    binary match {
      case literalPacketRegex(versionPart, intPart, rest) =>
        val integer = intPart.sliding(5, 5).map(_.drop(1)).mkString.fromBinary
        LiteralPacket(versionPart.fromBinary, integer)
          +: parse(rest)
      case operatorPacketBitLength(versionPart, typePart, bitLength, rest) =>
        val (operatorBits, restBits) = rest.splitAt(bitLength.fromBinary.toInt)
        OperatorPacket(versionPart.fromBinary, typePart, parse(operatorBits))
          +: parse(restBits)
      case operatorPacketSubpacketCountLength(versionPart, typePart, subpacketCount, rest) =>
        val (operatorPackets, restPackets) = parse(rest).splitAt(subpacketCount.fromBinary.toInt)
        OperatorPacket(versionPart.fromBinary, typePart, operatorPackets)
          +: restPackets
      case _ => Seq()
    }
  }
}

case class LiteralPacket(version: Long, value: Long) extends Packet(version) {
  override def getValue: Long = value
}

sealed abstract class OperatorPacket(private val version: Long, private val subpackets: Seq[Packet]) extends Packet(version)

object OperatorPacket {
  private val typeIdToFunctionMap: Map[String, (Long, Seq[Packet]) => OperatorPacket] = Map(
    "000" -> SumOperatorPacket.apply,
    "001" -> ProductOperatorPacket.apply,
    "010" -> MinimumOperatorPacket.apply,
    "011" -> MaximumOperatorPacket.apply,
    "101" -> GreaterThanOperatorPacket.apply,
    "110" -> LessThanOperatorPacket.apply,
    "111" -> EqualToOperatorPacket.apply
  )

  def apply(version: Long, typeId: String, packets: Seq[Packet]): OperatorPacket = {
    typeIdToFunctionMap(typeId).apply(version, packets)
  }

  def unapply(op: OperatorPacket): (Long, Seq[Packet]) = {
    (op.version, op.subpackets)
  }
}

case class SumOperatorPacket(version: Long, subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets) {
  override def getValue: Long = subpackets.map(_.getValue).sum
}

case class ProductOperatorPacket(version: Long, subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets) {
  override def getValue: Long = subpackets.map(_.getValue).product
}

case class MinimumOperatorPacket(version: Long, subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets) {
  override def getValue: Long = subpackets.map(_.getValue).min
}

case class MaximumOperatorPacket(version: Long, subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets) {
  override def getValue: Long = subpackets.map(_.getValue).max
}

case class GreaterThanOperatorPacket(version: Long, subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets) {
  override def getValue: Long = if (subpackets.head.getValue > subpackets(1).getValue) 1 else 0
}

case class LessThanOperatorPacket(version: Long, subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets) {
  override def getValue: Long = if (subpackets.head.getValue < subpackets(1).getValue) 1 else 0
}

case class EqualToOperatorPacket(version: Long, subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets) {
  override def getValue: Long = if (subpackets.head.getValue == subpackets(1).getValue) 1 else 0
}
