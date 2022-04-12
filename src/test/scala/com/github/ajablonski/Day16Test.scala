package com.github.ajablonski

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day16Test extends AnyFunSpec with Matchers with AocTestData(16) {
  describe("part1") {
    it("should return sum of version numbers") {
      Day16.part1(inputFile) shouldBe 31
    }
  }

  describe("parse") {
    it("should correctly parse literal packet") {
      Day16.parse("110100101111111000101") shouldBe Seq(LiteralPacket(6, 2021))
    }

    it("should correctly parse bit-length encoded packet") {
      Day16.parse(Seq("100", "101", "0", "000000000101100", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(OperatorPacket(4, "101", Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
    }

    it("should ignore trailing 0s") {
      Day16.parse("1101001011111110001010000000") shouldBe (Seq(LiteralPacket(6, 2021)))
    }

    it("should handle multi-level bit-length encoded packet") {
      Day16.parse(Seq("100", "101", "0", "000000010000100",
        Seq("100", "101", "0", "000000000101010", "110100101111111000101", "110100101111111000110").mkString,
        Seq("100", "101", "0", "000000000101010", "110100101111111000101", "110100101111111000110").mkString).mkString) shouldBe
        Seq(OperatorPacket(4, "101", Seq(
                    OperatorPacket(
                      4, "101",
                      Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))
                    ),
                    OperatorPacket(
                      4, "101",
                      Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))
                    ))))
    }

    it("should correctly parse packet-length encoded packet") {
      Day16.parse(Seq("100", "101", "1", "00000000010", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(OperatorPacket(4, "101", Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
    }

    it("should handle nested packet-length encoded packet") {
      Day16.parse(Seq("100", "101", "1", "00000000010",
        Seq("100", "101", "0", "000000000101010", "110100101111111000101", "110100101111111000110").mkString,
        Seq("100", "101", "0", "000000000101010", "110100101111111000101", "110100101111111000110").mkString).mkString) shouldBe
        Seq(OperatorPacket(4, "101", Seq(
                    OperatorPacket(4, "101", Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))),
                    OperatorPacket(4, "101", Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))))
    }

    it("should handle when only a few subpackets are part of the PacketCount operator") {
      Day16.parse(Seq("100", "101", "1", "00000000010",
        Seq("011", "110", "1", "00000000001", "110100101111111000101").mkString,
        "110100101111111000110"
      ).mkString) shouldBe Seq(
        OperatorPacket(4, "101", Seq(
                    OperatorPacket(
                      3, "110", Seq(LiteralPacket(6, 2021))
                    ),
                    LiteralPacket(6, 2022)
                  ))
      )
    }
  }
}
