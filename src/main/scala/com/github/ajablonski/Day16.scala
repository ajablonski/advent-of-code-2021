package com.github.ajablonski

import scala.annotation.tailrec
import scala.runtime.RichInt
import java.lang.Long as JLong

object Day16 extends AocProblem[Int, Int] {
  private val literalNumberDigitLength = 16
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
      case OperatorPacketBitLength(version, _, _, subpackets) => version + subpackets.map(sumVersions).sum
      case OperatorPacketPacketCount(version, _, _, subpackets) => version + subpackets.map(sumVersions).sum
    }
  }

  def parse(binary: String): Seq[Packet] = {
    binary match {
      case literalPacketRegex(versionPart, intPart, rest) =>
        val integer = intPart.sliding(5, 5).map(_.drop(1)).mkString.fromBinary
        LiteralPacket(versionPart.fromBinary, integer)
          +: parse(rest)
      case operatorPacketBitLength(versionPart, typePart, bitLength, rest) =>
        OperatorPacketBitLength(versionPart.fromBinary, typePart, bitLength.fromBinary, parse(rest.take(bitLength.fromBinary.toInt)))
          +: parse(rest.drop(bitLength.fromBinary.toInt))
      case operatorPacketSubpacketCountLength(versionPart, typePart, subpacketCount, rest) =>
        val otherPackets = parse(rest)
        OperatorPacketPacketCount(versionPart.fromBinary, typePart, subpacketCount.fromBinary, otherPackets.take(subpacketCount.fromBinary.toInt))
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

sealed trait Packet(version: Long, typeId: String) {
  //  def toBinaryString: String
}

case class LiteralPacket(version: Long, value: Long) extends Packet(version, "100") {
  //  override def toBinaryString: String = Seq(version.toBinaryString.padBinary(3), ).mkString
}

sealed abstract class OperatorPacket(version: Long, typeId: String, lengthTypeId: Byte, subpackets: Seq[Packet]) extends Packet(version, typeId)

case class OperatorPacketBitLength(version: Long, typeId: String, bitLength: Long, subpackets: Seq[Packet]) extends OperatorPacket(version, typeId, 0, subpackets) {
}

case class OperatorPacketPacketCount(version: Long, typeId: String, subpacketCount: Long, subpackets: Seq[Packet]) extends OperatorPacket(version, typeId, 1, subpackets) {
}