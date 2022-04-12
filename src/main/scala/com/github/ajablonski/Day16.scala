package com.github.ajablonski

import scala.annotation.tailrec
import scala.runtime.RichInt
import java.lang.Long as JLong

object Day16 extends AocProblem[Int, Int] {
  private val literalPacketRegex = raw"([01]{3})100((?:1[01]{4})*0[01]{4})([01]*)".r
  private val operatorPacketBitLength = raw"([01]{3})([01]{3})0([01]{15})([01]*)".r
  private val operatorPacketSubpacketCountLength = raw"([01]{3})([01]{3})1([01]{11})([01]*)".r
  private val trailing0s = raw"0*".r

  override def part1(filename: String): Int = {
    import Day16.toBinary
    val packetString = getRawData(filename).head.toBinary

    val packet = parse(packetString)

    sumVersions(packet.head).toInt
  }

  override def part2(filename: String): Int = ???

  def sumVersions(packet: Packet): Long = {
    packet match {
      case LiteralPacket(version, _) => version
      case OperatorPacket(version, subpackets) => version + subpackets.map(sumVersions).sum
    }
  }

  def parse(binary: String): Seq[Packet] = {
    binary match {
      case literalPacketRegex(versionPart, intPart, rest) =>
        val integer = intPart.sliding(5, 5).map(_.drop(1)).mkString.fromBinary
        LiteralPacket(versionPart.fromBinary, integer)
          +: parse(rest)
      case operatorPacketBitLength(versionPart, typePart, bitLength, rest) =>
        OperatorPacket(versionPart.fromBinary, typePart, parse(rest.take(bitLength.fromBinary.toInt)))
          +: parse(rest.drop(bitLength.fromBinary.toInt))
      case operatorPacketSubpacketCountLength(versionPart, typePart, subpacketCount, rest) =>
        val otherPackets = parse(rest)
        OperatorPacket(versionPart.fromBinary, typePart, otherPackets.take(subpacketCount.fromBinary.toInt))
          +: otherPackets.drop(subpacketCount.fromBinary.toInt)
      case trailing0s => Seq()
    }
  }

  extension (s: String) {
    def toBinary: String = {
      s.split("").map(s => RichInt(Integer.parseInt(s, 16)).toBinaryString.reverse.padTo(4, '0').reverse).mkString
    }

    def fromBinary: Long = {
      JLong.parseLong(s, 2)
    }

    def padBinary(characters: Int): String = {
      s.reverse.padTo(characters, '0').reverse
    }
  }
}

sealed trait Packet(version: Long) {
  def getValue: Long
}

case class LiteralPacket(version: Long, value: Long) extends Packet(version) {
  override def getValue: Long = value
}

sealed abstract class OperatorPacket(val version: Long, val subpackets: Seq[Packet]) extends Packet(version) {
  override def getValue: Long = 0
}

case class SumOperatorPacket(override val version: Long, override val subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets)
case class ProductOperatorPacket(override val version: Long, override val subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets)
case class MinimumOperatorPacket(override val version: Long, override val subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets)
case class MaximumOperatorPacket(override val version: Long, override val subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets)
case class GreaterThanOperatorPacket(override val version: Long, override val subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets)
case class LessThanOperatorPacket(override val version: Long, override val subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets)
case class EqualToOperatorPacket(override val version: Long, override val subpackets: Seq[Packet]) extends OperatorPacket(version, subpackets)

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