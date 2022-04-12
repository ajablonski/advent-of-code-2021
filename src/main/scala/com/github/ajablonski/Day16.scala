package com.github.ajablonski

import com.github.ajablonski.day_16.{LiteralPacket, OperatorPacket, Packet, BinaryStringExtensions}

import scala.annotation.tailrec
import scala.runtime.RichInt
import java.lang.Long as JLong

object Day16 extends AocProblem[Int, Long] {

  override def part1(filename: String): Int = {
    import BinaryStringExtensions.hexToBinary
    val packetString = getRawData(filename).head.hexToBinary

    val packet = Packet.parse(packetString)

    sumVersions(packet.head).toInt
  }

  override def part2(filename: String): Long = {
    import BinaryStringExtensions.hexToBinary

    val packetString = getRawData(filename).head.hexToBinary

    val packet = Packet.parse(packetString)

    packet.head.getValue
  }

  def sumVersions(packet: Packet): Long = {
    packet match {
      case LiteralPacket(version, _) => version
      case OperatorPacket(version, subpackets) => version + subpackets.map(sumVersions).sum
    }
  }
}
