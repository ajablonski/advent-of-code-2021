package com.github.ajablonski

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.ajablonski.day_13.FoldInstruction
import com.github.ajablonski.day_13.X
import com.github.ajablonski.day_13.Y

class Day13Test extends AnyFlatSpec with Matchers with AocTestData(13) {
  "part1" should "return count of visible dots after first fold" in {
    Day13.part1(inputFile) shouldBe 17
  }

  "part2" should "return string that shows diagram" in {
    Day13.part2(inputFile) shouldBe
      """#####
        |#...#
        |#...#
        |#...#
        |#####
        |""".stripMargin
  }

  "parse" should "correctly parse file" in {
    Day13.parse(Seq(
      "6,10",
      "0,14",
      "9,10",
      "0,3",
      "",
      "fold along y=7",
      "fold along x=5"
    )) shouldBe(Set(
      (6, 10),
      (0, 14),
      (9, 10),
      (0, 3),
    ), Seq(FoldInstruction(Y, 7), FoldInstruction(X, 5)))
  }

  "applyFold" should "fold entries over on each other across a Y= line" in {
    Day13.applyFold(Set(
      (6, 10),
      (0, 2)
    ), FoldInstruction(Y, 7)) shouldBe Set(
      (6, 4),
      (0, 2)
    )
  }

  it should "fold entries over on each other across an X= line" in {
    Day13.applyFold(Set(
      (6, 10),
      (0, 2)
    ), FoldInstruction(X, 3)) shouldBe Set(
      (0, 10),
      (0, 2)
    )
  }
}
