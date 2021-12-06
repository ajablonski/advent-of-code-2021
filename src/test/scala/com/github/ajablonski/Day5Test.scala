package com.github.ajablonski

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.ajablonski.day_5._

class Day5Test extends AnyFlatSpec with Matchers with AocTestData(5) {
  "part1" should "count the number of points with more than 1 vent" in {
    Day5.part1(inputFile) shouldBe 5
  }

  "part2" should "count the number of points with more than 1 vent counting diagonal lines" in {
    Day5.part2(inputFile) shouldBe 12
  }

  "parse" should "return lines" in {
    Day5.parse(
      Seq("0,9 -> 5,9",
        "0,8 -> 8,0",
        "2,1 -> 2,2")
    ) shouldBe Seq(
      HorizontalLine(9, 0, 5),
      DiagonalLine(0, 8, 8, 0),
      VerticalLine(2, 1, 2)
    )
  }

  it should "correct order of entries when reversed" in {
    Day5.parse(
      Seq("5,9 -> 0,9",
        "8,0 -> 0,8",
        "2,2 -> 2,1")
    ) shouldBe Seq(
      HorizontalLine(9, 0, 5),
      DiagonalLine(0, 8, 8, 0),
      VerticalLine(2, 1, 2)
    )
  }
}
