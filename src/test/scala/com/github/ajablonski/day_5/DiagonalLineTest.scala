package com.github.ajablonski.day_5

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DiagonalLineTest extends AnyFlatSpec with Matchers {
  "getPointsOnLine" should "work for a positively-sloped line" in {
    DiagonalLine(0, 0, 2, 2).pointsOnLine shouldBe Seq((0, 0), (1, 1), (2, 2))
    DiagonalLine(1, 0, 3, 2).pointsOnLine shouldBe Seq((1, 0), (2, 1), (3, 2))
  }

  it should "work for a negatively sloped line" in {
    DiagonalLine(0, 2, 2, 0).pointsOnLine shouldBe Seq((0, 2), (1, 1), (2, 0))
    DiagonalLine(1, 2, 3, 0).pointsOnLine shouldBe Seq((1, 2), (2, 1), (3, 0))
  }
}
