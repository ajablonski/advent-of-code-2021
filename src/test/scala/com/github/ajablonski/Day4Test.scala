package com.github.ajablonski

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day4Test extends AnyFlatSpec with Matchers with AocTestData(4) {
  "part1" should "find the winning board and return sum of unmarked numbers times last number called" in {
    Day4.part1(inputFile) shouldBe 4512
  }
  
  "part2" should "find the last winning board and return the sum of unmarked numbers times last number called" in {
    Day4.part2(inputFile) shouldBe 1924
  }
}
