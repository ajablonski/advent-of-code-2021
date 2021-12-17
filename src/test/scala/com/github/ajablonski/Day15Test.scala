package com.github.ajablonski

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.ajablonski.util.NumberGrid

class Day15Test extends AnyFlatSpec with Matchers with AocTestData(15) {
  "part1" should "return the risk of the lowest risk path" in {
    Day15.part1(inputFile) shouldBe 40
  }

  "part2" should "return the risk of the expanded grid" in {
    Day15.part2(inputFile) shouldBe 315
  }

  "findLeastRiskPath" should "return the risk-weighted shortest path" in {
    Day15.findLeastRiskPath(Map(
      (0, 0) -> 1, (0, 1) -> 1, (0, 2) -> 6,
      (1, 0) -> 1, (1, 1) -> 3, (1, 2) -> 8,
      (2, 0) -> 2, (2, 1) -> 1, (2, 2) -> 6,
    )) shouldBe Seq(
      (0, 0) -> 1,
      (1, 0) -> 1,
      (2, 0) -> 2,
      (2, 1) -> 1,
      (2, 2) -> 6
    )
  }

  "expandMap" should "expand the raw input map 4 more times in each direction" in {
    Day15.expandMap(Seq(
      "123",
      "456",
      "789"
    )) shouldBe Seq(
      "123234345456567",
      "456567678789891",
      "789891912123234",
      "234345456567678",
      "567678789891912",
      "891912123234345",
      "345456567678789",
      "678789891912123",
      "912123234345456",
      "456567678789891",
      "789891912123234",
      "123234345456567",
      "567678789891912",
      "891912123234345",
      "234345456567678"
    )
  }
}
