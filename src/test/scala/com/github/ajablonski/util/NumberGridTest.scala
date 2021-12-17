package com.github.ajablonski.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NumberGridTest extends AnyFlatSpec with Matchers {

  import NumberGrid.findAdjacentPoints

  "parse" should "returned indexed view of data" in {
    NumberGrid.parse(Seq(
      "219",
      "398",
      "985"
    )) shouldBe Map(
      (0, 0) -> 2, (0, 1) -> 1, (0, 2) -> 9,
      (1, 0) -> 3, (1, 1) -> 9, (1, 2) -> 8,
      (2, 0) -> 9, (2, 1) -> 8, (2, 2) -> 5
    )
  }

  "getAdjacentPoints" should "return all adjacent points with risk value" in {
    Map(
      (0, 0) -> 1, (0, 1) -> 1, (0, 2) -> 6,
      (1, 0) -> 1, (1, 1) -> 3, (1, 2) -> 8,
      (2, 2) -> 2, (2, 1) -> 1, (2, 2) -> 6,
    ).findAdjacentPoints((1, 1)) shouldBe Set(
      (0, 1) -> 1,
      (1, 2) -> 8,
      (1, 0) -> 1,
      (2, 1) -> 1
    )
  }

  it should "return only adjacent cells that exist if no default is provided" in {
    Map(
      (0, 0) -> 1, (0, 1) -> 1, (0, 2) -> 6,
      (1, 0) -> 1, (1, 1) -> 3, (1, 2) -> 8,
      (2, 2) -> 2, (2, 1) -> 1, (2, 2) -> 6,
    ).findAdjacentPoints((0, 0)) shouldBe Set(
      (0, 1) -> 1,
      (1, 0) -> 1
    )
    Map(
      (0, 0) -> 1, (0, 1) -> 1, (0, 2) -> 6,
      (1, 0) -> 1, (1, 1) -> 3, (1, 2) -> 8,
      (2, 2) -> 2, (2, 1) -> 1, (2, 2) -> 6,
    ).findAdjacentPoints((0, 1)) shouldBe Set(
      (0, 0) -> 1,
      (1, 1) -> 3,
      (0, 2) -> 6
    )
    Map(
      (0, 0) -> 1, (0, 1) -> 1, (0, 2) -> 6,
      (1, 0) -> 1, (1, 1) -> 3, (1, 2) -> 8,
      (2, 2) -> 2, (2, 1) -> 1, (2, 2) -> 6,
    ).findAdjacentPoints((2, 2)) shouldBe Set(
      (2, 1) -> 1,
      (1, 2) -> 8
    )
  }

  it should "return default values when provided" in {
    Map(
      (0, 0) -> 1, (0, 1) -> 1, (0, 2) -> 6,
      (1, 0) -> 1, (1, 1) -> 3, (1, 2) -> 8,
      (2, 2) -> 2, (2, 1) -> 1, (2, 2) -> 6,
    ).findAdjacentPoints((0, 0), Some(100)) shouldBe Set(
      (0, 1) -> 1,
      (0, -1) -> 100,
      (1, 0) -> 1,
      (-1, 0) -> 100
    )
  }
}
