package com.github.ajablonski.day_5

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SeafloorTest extends AnyFlatSpec with Matchers {
  "mark" should "update Seafloor with a horizontal line" in {
    Seafloor(
      Seq(
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0)
      ))
      .mark(HorizontalLine(1, 1, 3)) shouldBe
      Seafloor(
        Seq(
          Seq(0, 0, 0, 0),
          Seq(0, 1, 1, 1),
          Seq(0, 0, 0, 0),
          Seq(0, 0, 0, 0)
        ))
  }

  it should "update Seafloor with a vertical line" in {
    Seafloor(
      Seq(
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0)
      ))
      .mark(VerticalLine(1, 1, 3)) shouldBe
      Seafloor(
        Seq(
          Seq(0, 0, 0, 0),
          Seq(0, 1, 0, 0),
          Seq(0, 1, 0, 0),
          Seq(0, 1, 0, 0)
        ))
  }

  it should "increase counts when lines overlap" in {
    Seafloor(
      Seq(
        Seq(0, 0, 0, 0),
        Seq(0, 1, 0, 0),
        Seq(0, 1, 0, 0),
        Seq(0, 1, 0, 0)
      )).mark(HorizontalLine(1, 1, 3)) shouldBe
      Seafloor(
        Seq(
          Seq(0, 0, 0, 0),
          Seq(0, 2, 1, 1),
          Seq(0, 1, 0, 0),
          Seq(0, 1, 0, 0)
        ))
  }

  it should "expand the seafloor if not big enough in X for a horizontal line" in {
    Seafloor(
      Seq(
        Seq(0, 0),
        Seq(0, 0)))
      .mark(HorizontalLine(1, 2, 7)) shouldBe
      Seafloor(
        Seq(
          Seq(0, 0, 0, 0, 0, 0, 0, 0),
          Seq(0, 0, 1, 1, 1, 1, 1, 1)
        ))
  }

  it should "expand the seafloor if not big enough in X for a vertical line" in {
    Seafloor(
      Seq(
        Seq(0, 0),
        Seq(0, 0)))
      .mark(VerticalLine(5, 0, 1)) shouldBe
      Seafloor(
        Seq(
          Seq(0, 0, 0, 0, 0, 1),
          Seq(0, 0, 0, 0, 0, 1)
        ))
  }

  it should "expand the seafloor if not big enough in Y for a vertical line" in {
    Seafloor(
      Seq(
        Seq(0, 0),
        Seq(0, 0)))
      .mark(VerticalLine(1, 2, 7)) shouldBe
      Seafloor(
        Seq(
          Seq(0, 0),
          Seq(0, 0),
          Seq(0, 1),
          Seq(0, 1),
          Seq(0, 1),
          Seq(0, 1),
          Seq(0, 1),
          Seq(0, 1)
        ))
  }

  it should "expand the seafloor if not big enough in Y for a horizontal line" in {
    Seafloor(
      Seq(
        Seq(0, 0),
        Seq(0, 0)))
      .mark(HorizontalLine(3, 0, 1)) shouldBe
      Seafloor(
        Seq(
          Seq(0, 0),
          Seq(0, 0),
          Seq(0, 0),
          Seq(1, 1),
        ))
  }

  "markWithDiagonals" should "also mark horizontal and vertical lines" in {
    Seafloor(
      Seq(
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0)
      ))
      .markWithDiagonals(VerticalLine(1, 1, 3))
      .markWithDiagonals(HorizontalLine(1, 1, 3)) shouldBe
      Seafloor(
        Seq(
          Seq(0, 0, 0, 0),
          Seq(0, 2, 1, 1),
          Seq(0, 1, 0, 0),
          Seq(0, 1, 0, 0)
        ))
  }

  it should "mark line with increasing coordinates" in {
    Seafloor(
      Seq(
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0)
      ))
      .markWithDiagonals(DiagonalLine(0, 0, 2, 2)) shouldBe
      Seafloor(
        Seq(
          Seq(1, 0, 0, 0),
          Seq(0, 1, 0, 0),
          Seq(0, 0, 1, 0),
          Seq(0, 0, 0, 0)
        ))
  }

  it should "mark line with decreasing coordinate" in {
    Seafloor(
      Seq(
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0)
      ))
      .markWithDiagonals(DiagonalLine(0, 2, 2, 0)) shouldBe
      Seafloor(
        Seq(
          Seq(0, 0, 1, 0),
          Seq(0, 1, 0, 0),
          Seq(1, 0, 0, 0),
          Seq(0, 0, 0, 0)
        ))
  }
}
