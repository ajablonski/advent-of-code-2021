package com.github.ajablonski

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14Test extends AnyFlatSpec with Matchers with AocTestData(14) {
  "part1" should "return difference between most and least common letter after 10 iterations" in {
    Day14.part1(inputFile) shouldBe 1588
  }

  "part2" should "return difference between most and least common letter after 40 iterations" in {
    Day14.part2(inputFile) shouldBe BigInt("2188189693529")
  }

  "parse" should "return base sequence and rules to match" in {
    Day14.parse(Seq(
      "NNCB",
      "",
      "CH -> B",
      "HH -> N",
      "CB -> H",
      "NH -> C",
      "HB -> C",
      "HC -> B",
      "HN -> C",
      "NN -> C",
      "BH -> H",
      "NC -> B",
      "NB -> B",
      "BN -> B",
      "BB -> N",
      "BC -> B",
      "CC -> N",
      "CN -> C"
    )) shouldBe ((Map("NN" -> 1, "NC" -> 1, "CB" -> 1), "CB"), Map(
      "CH" -> "B",
      "HH" -> "N",
      "CB" -> "H",
      "NH" -> "C",
      "HB" -> "C",
      "HC" -> "B",
      "HN" -> "C",
      "NN" -> "C",
      "BH" -> "H",
      "NC" -> "B",
      "NB" -> "B",
      "BN" -> "B",
      "BB" -> "N",
      "BC" -> "B",
      "CC" -> "N",
      "CN" -> "C"
    ))
  }

  "step" should "apply all rules to extend string" in {
    Day14.step(Map("NN" -> 1, "NC" -> 1, "CB" -> 1), "CB", Map(
      "CH" -> "B",
      "HH" -> "N",
      "CB" -> "H",
      "NH" -> "C",
      "HB" -> "C",
      "HC" -> "B",
      "HN" -> "C",
      "NN" -> "C",
      "BH" -> "H",
      "NC" -> "B",
      "NB" -> "B",
      "BN" -> "B",
      "BB" -> "N",
      "BC" -> "B",
      "CC" -> "N",
      "CN" -> "C"
    )) shouldBe {
      (Map(
        "NC" -> 1,
        "CN" -> 1,
        "NB" -> 1,
        "BC" -> 1,
        "CH" -> 1,
        "HB" -> 1
      ), "HB")
    }
  }

  it should "work when entry appears multiple times" in {
    Day14.stepN(Map("NN" -> 1, "NC" -> 1, "CB" -> 1), "CB", Map(
      "CH" -> "B",
      "HH" -> "N",
      "CB" -> "H",
      "NH" -> "C",
      "HB" -> "C",
      "HC" -> "B",
      "HN" -> "C",
      "NN" -> "C",
      "BH" -> "H",
      "NC" -> "B",
      "NB" -> "B",
      "BN" -> "B",
      "BB" -> "N",
      "BC" -> "B",
      "CC" -> "N",
      "CN" -> "C"
    ), 2) shouldBe (Map("NB" -> 2,
    "BC" -> 2,
    "CC" -> 1,
    "CN" -> 1,
    "BB" -> 2,
    "CB" -> 2,
    "BH" -> 1,
    "HC" -> 1), "CB")
  }
}
