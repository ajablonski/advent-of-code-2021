package com.github.ajablonski

import com.github.ajablonski.day_8.DigitFinders
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day8Test extends AnyFlatSpec with Matchers with AocTestData(8) {
  "part1" should "return count of 1s, 4s, 7s, and 8s" in {
    Day8.part1(inputFile) shouldBe 26
  }

  "part2" should "sum up all of the decoded values" in {
    Day8.part2(inputFile) shouldBe 61229
  }

  "parseLine" should "return input and output segment sets" in {
    Day8.parseLine("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe") shouldBe(
      Set(Set("be".split(""): _*), Set("cfbegad".split(""): _*), Set("cbdgef".split(""): _*), Set("fgaecd".split(""): _*), Set("cgeb".split(""): _*), Set("fdcge".split(""): _*), Set("agebfd".split(""): _*), Set("fecdb".split(""): _*), Set("fabcd".split(""): _*), Set("edb".split(""): _*)),
      Seq(Set("fdgacbe".split(""): _*), Set("cefdb".split(""): _*), Set("cefbgd".split(""): _*), Set("gcbe".split(""): _*))
    )
  }

  "translateOutput" should "decode known outputs" in {
    val outputToTranslate = Day8
      .parseLine("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | be bceg bed abcdefg")
      ._2

    val digitMap = Map(
      1 -> Set("b", "e"),
      4 -> Set("b", "c", "e", "g"),
      7 -> Set("b", "e", "d"),
      8 -> Set("a", "b", "c", "d", "e", "f", "g"),
    )
    Day8.translateOutput(outputToTranslate, digitMap) shouldBe "1478"
  }

  it should "replace unknown outputs with ?" in {
    val outputToTranslate = Day8
      .parseLine("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | bd a x abcdefghij")
      ._2

    val digitMap = Map(
      1 -> Set("b", "e"),
      4 -> Set("b", "c", "e", "g"),
      7 -> Set("b", "e", "d"),
      8 -> Set("a", "b", "c", "d", "e", "f", "g"),
    )
    Day8.translateOutput(outputToTranslate, digitMap) shouldBe "????"
  }

  "decodeLine" should "return result of decoding entire line" in {
    Day8.decodeLine("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe") shouldBe "8394"
  }
}
