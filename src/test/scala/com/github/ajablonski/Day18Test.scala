package com.github.ajablonski

import com.github.ajablonski.day_18.{SnailfishNumber => SFN}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class Day18Test extends AnyFunSpec with should.Matchers with AocTestData(18) {
  describe("part1") {
    it("should calculate the magnitude of the final snailfish number") {
      Day18.part1(inputFile) shouldBe 4140
    }
  }

  describe("part2") {
    it("should calculate the magnitude of the largest possible sum of two snailfish numbers in the list") {
      Day18.part2(inputFile) shouldBe 3993
    }
  }

  describe("parse") {
    it("should handle simple input values") {
      Day18.parseInput(Seq("[0,1]")) should equal(Seq(SFN(0, 1)))
    }

    it("should handle complex input values containing a simple number") {
      Day18.parseInput(Seq("[0,[1,2]]")) should equal(Seq(SFN(0, SFN(1, 2))))
      Day18.parseInput(Seq("[[0,1],2]")) should equal(Seq(SFN(SFN(0, 1), 2)))
    }

    it("should handle single-level nested complex input") {
      Day18.parseInput(Seq("[[0,1],[2,3]]")) should equal(Seq(
        SFN(
          SFN(0, 1),
          SFN(2, 3))))
    }

    it("should handle more complex input") {
      Day18.parseInput(Seq("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]")) should equal(Seq(
        SFN(
          SFN(SFN(0, SFN(5, 8)), SFN(SFN(1, 7), SFN(9, 6))),
          SFN(SFN(4, SFN(1,2)), SFN(SFN(1, 4), 2))
        )
      ))
    }
  }
}
