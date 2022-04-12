package com.github.ajablonski

import com.github.ajablonski.day_16.LiteralPacket
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
}
