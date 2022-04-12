package com.github.ajablonski

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

//noinspection DeprecatedAlphanumericInfixCall
class Day16Test extends AnyFlatSpec with Matchers with AocTestData(16) {
  "part1" should "return sum of version numbers" in {
    Day16.part1(inputFile) shouldBe 31
  }

  "parse" should "correctly parse literal packet" in {
    Day16.parse("110100101111111000101") shouldBe Seq(LiteralPacket(6, 2021))
  }

  it should "correctly parse bit-length encoded packet" in {
    Day16.parse(Seq("100", "101", "0", "000000000101100", "110100101111111000101", "110100101111111000110").mkString) shouldBe
      Seq(OperatorPacketBitLength(
        4, "101", 44,
        Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))
      ))
  }

  it should "ignore trailing 0s" in {
    Day16.parse("1101001011111110001010000000") shouldBe (Seq(LiteralPacket(6, 2021)))
  }

  it should "handle multi-level bit-length encoded packet" in {
    Day16.parse(Seq("100", "101", "0", "000000010000100",
      Seq("100", "101", "0", "000000000101010", "110100101111111000101", "110100101111111000110").mkString,
      Seq("100", "101", "0", "000000000101010", "110100101111111000101", "110100101111111000110").mkString).mkString) shouldBe
      Seq(OperatorPacketBitLength(
        4, "101", 132,
        Seq(
          OperatorPacketBitLength(
            4, "101", 42,
            Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))
          ),
          OperatorPacketBitLength(
            4, "101", 42,
            Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))
          ))
      ))
  }

  it should "correctly parse packet-length encoded packet" in {
    Day16.parse(Seq("100", "101", "1", "00000000010", "110100101111111000101", "110100101111111000110").mkString) shouldBe
      Seq(OperatorPacketPacketCount(
        4, "101", 2,
        Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))
      ))
  }

  it should "handle nested packet-length encoded packet" in {
    Day16.parse(Seq("100", "101", "1", "00000000010",
      Seq("100", "101", "0", "000000000101010", "110100101111111000101", "110100101111111000110").mkString,
      Seq("100", "101", "0", "000000000101010", "110100101111111000101", "110100101111111000110").mkString).mkString) shouldBe
      Seq(OperatorPacketPacketCount(
        4, "101", 2,
        Seq(
          OperatorPacketBitLength(
            4, "101", 42,
            Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))
          ),
          OperatorPacketBitLength(
            4, "101", 42,
            Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))
          ))
      ))
  }

  it should "handle when only a few subpackets are part of the PacketCount operator" in {
    Day16.parse(Seq("100", "101", "1", "00000000010",
      Seq("011", "110", "1", "00000000001", "110100101111111000101").mkString,
      "110100101111111000110"
    ).mkString) shouldBe Seq(
      OperatorPacketPacketCount(
        4, "101", 2,
        Seq(
          OperatorPacketPacketCount(
            3, "110", 1, Seq(LiteralPacket(6, 2021))
          ),
          LiteralPacket(6, 2022)
        )
      )
    )
  }
}
