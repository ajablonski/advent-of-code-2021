package com.github.ajablonski

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3Test extends AnyFlatSpec with Matchers {
  "part 1" should "return the power consumption as the product of gamma and epsilon" in {
    Day3.part1(getClass.getResource("/samples/day3.txt").getFile) shouldBe 198
  }

  "gamma" should "calculate number from most significant bits" in {
    Day3.gamma(Seq(
      "000",
      "111",
      "010"
    )) shouldBe 2
  }

  "epsilon" should "calculate number from least significant bits" in {
    Day3.epsilon(Seq(
      "000",
      "111",
      "010"
    )) shouldBe 5
  }

  "part 2" should "return the C02 scrubber rating as the product of oxygen generator rating and C02 scrubber rating" in {
    Day3.part2(getClass.getResource("/samples/day3.txt").getFile) shouldBe 230
  }

  "oxygenGeneratorRating" should "return single entry from single entry list" in {
    Day3.oxygenGeneratorRating(Seq("000")) shouldBe 0
  }

  it should "filter based on most common bit using 1 when counts are equal" in {
    Day3.oxygenGeneratorRating(Seq("000", "111", "110", "101")) shouldBe 7
  }

  "co2ScrubberRating" should "return single entry from single entry list" in {
    Day3.co2ScrubberRating(Seq("000")) shouldBe 0
  }

  it should "filter based on least common bit when there is no tie in first position" in {
    Day3.co2ScrubberRating(Seq("000", "111", "110")) shouldBe 0
  }

  it should "filter based on least common bit when there is a tie in determining the least common bit in first position" in {
    Day3.co2ScrubberRating(Seq("000", "111")) shouldBe 0
  }

  it should "continue filtering on subsequent digits" in {
    Day3.co2ScrubberRating(Seq("000001", "000000", "000010")) shouldBe 2
  }

  "parse raw input" should "return sequent of maps with bit counts counts by position" in {
    Day3.parse(Seq(
      "000",
      "111",
      "010"
    )) shouldBe Seq(Map("0" -> 2, "1" -> 1), Map("0" -> 1, "1" -> 2), Map("0" -> 2, "1" -> 1))
  }
}
