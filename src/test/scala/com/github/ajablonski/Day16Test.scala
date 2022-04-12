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

  describe("part2") {
    it("should calculate the value after applying operators") {
      Day16.part2(inputFile) shouldBe 54
    }
  }

  describe("parse") {
    it("should correctly parse literal packet") {
      Day16.parse("110100101111111000101") shouldBe Seq(LiteralPacket(6, 2021))
    }

    it("should correctly parse bit-length encoded packet") {
      Day16.parse(Seq("100", "101", "0", "000000000101100", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(GreaterThanOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
    }

    it("should ignore trailing 0s") {
      Day16.parse("1101001011111110001010000000") shouldBe (Seq(LiteralPacket(6, 2021)))
    }

    it("should correctly parse a sum operator") {
      Day16.parse(Seq("100", "000", "0", "000000000101100", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(SumOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
      Day16.parse(Seq("100", "000", "1", "00000000010", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(SumOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
    }

    it("should correctly parse a product operator") {
      Day16.parse(Seq("100", "001", "0", "000000000101100", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(ProductOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
      Day16.parse(Seq("100", "001", "1", "00000000010", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(ProductOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
    }

    it("should correctly parse a minimum operator") {
      Day16.parse(Seq("100", "010", "0", "000000000101100", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(MinimumOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
      Day16.parse(Seq("100", "010", "1", "00000000010", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(MinimumOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
    }

    it("should correctly parse a maximum operator") {
      Day16.parse(Seq("100", "011", "0", "000000000101100", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(MaximumOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
      Day16.parse(Seq("100", "011", "1", "00000000010", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(MaximumOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
    }

    it("should correctly parse a greater than operator") {
      Day16.parse(Seq("100", "101", "0", "000000000101100", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(GreaterThanOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
      Day16.parse(Seq("100", "101", "1", "00000000010", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(GreaterThanOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
    }

    it("should correctly parse a less than operator") {
      Day16.parse(Seq("100", "110", "0", "000000000101100", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(LessThanOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
      Day16.parse(Seq("100", "110", "1", "00000000010", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(LessThanOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
    }

    it("should correctly parse an equal to operator") {
      Day16.parse(Seq("100", "111", "0", "000000000101100", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(EqualToOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
      Day16.parse(Seq("100", "111", "1", "00000000010", "110100101111111000101", "110100101111111000110").mkString) shouldBe
        Seq(EqualToOperatorPacket(4, Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))))
    }

    it("should handle multi-level bit-length encoded packet") {
      Day16.parse(Seq("100", "101", "0", "000000010000100",
        Seq("100", "101", "0", "000000000101010", "110100101111111000101", "110100101111111000110").mkString,
        Seq("100", "101", "0", "000000000101010", "110100101111111000101", "110100101111111000110").mkString).mkString) shouldBe
        Seq(OperatorPacket(4, "101", Seq(
          OperatorPacket(
            4,
            "101",
            Seq(LiteralPacket(6, 2021), LiteralPacket(6, 2022))
          ),
          OperatorPacket(
            4,
            "101",
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
        GreaterThanOperatorPacket(4, Seq(
          LessThanOperatorPacket(
            3, Seq(LiteralPacket(6, 2021))
          ),
          LiteralPacket(6, 2022)
        ))
      )
    }
  }

  describe("getValue") {
    describe("for literal") {
      it("should return the literal's value") {
        LiteralPacket(1, 200).getValue shouldBe 200
      }
    }

    describe("sum") {
      it("should correctly operate on two literal values") {
        SumOperatorPacket(1, Seq(LiteralPacket(1, 200), LiteralPacket(1, 100))).getValue shouldBe 300
      }
    }

    describe("product") {
      it("should correctly operate on two literal values") {
        ProductOperatorPacket(1, Seq(LiteralPacket(1, 200), LiteralPacket(1, 100))).getValue shouldBe 20000
      }
    }

    describe("minimum") {
      it("should correctly operate on two literal values") {
        MinimumOperatorPacket(1, Seq(LiteralPacket(1, 200), LiteralPacket(1, 100))).getValue shouldBe 100
      }
    }

    describe("maximum") {
      it("should correctly operate on two literal values") {
        MaximumOperatorPacket(1, Seq(LiteralPacket(1, 200), LiteralPacket(1, 100))).getValue shouldBe 200
      }
    }

    describe("greaterThan") {
      it("should correctly operate on two literal values") {
        GreaterThanOperatorPacket(1, Seq(LiteralPacket(1, 200), LiteralPacket(1, 100))).getValue shouldBe 1
      }
    }

    describe("lessThan") {
      it("should correctly operate on two literal values") {
        LessThanOperatorPacket(1, Seq(LiteralPacket(1, 200), LiteralPacket(1, 100))).getValue shouldBe 0
      }
    }

    describe("equalTo") {
      it("should correctly operate on two literal values") {
        EqualToOperatorPacket(1, Seq(LiteralPacket(1, 200), LiteralPacket(1, 100))).getValue shouldBe 0
      }
    }

    it("should handle complex values") {
      import Day16.toBinary

      Day16.parse("C200B40A82".toBinary).head.getValue shouldBe 3
      Day16.parse("04005AC33890".toBinary).head.getValue shouldBe 54
      Day16.parse("880086C3E88112".toBinary).head.getValue shouldBe 7
      Day16.parse("CE00C43D881120".toBinary).head.getValue shouldBe 9
      Day16.parse("D8005AC2A8F0".toBinary).head.getValue shouldBe 1
      Day16.parse("F600BC2D8F".toBinary).head.getValue shouldBe 0
      Day16.parse("9C005AC2F8F0".toBinary).head.getValue shouldBe 0
      Day16.parse("9C0141080250320F1802104A08".toBinary).head.getValue shouldBe 1
    }
  }
}
