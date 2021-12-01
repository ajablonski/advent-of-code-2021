package com.github.ajablonski

import com.github.ajablonski.Day1
import org.scalatest.*
import org.scalatest.matchers.*

import scala.io.Source


class Day1Test extends flatspec.AnyFlatSpec with should.Matchers {
  "parsing input" should "read in input into list of strings" in {
    Day1.parseInput(getClass.getResource("/samples/day1.txt").getFile).toList shouldEqual
      List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
  }

  "part1" should "count distances that have increased" in {
    Day1.part1(getClass.getResource("/samples/day1.txt").getFile) shouldEqual 7
  }

  "part2" should "calculate sliding windows of size 3 that have increased over previous windows" in {
    Day1.part2(getClass.getResource("/samples/day1.txt").getFile) shouldEqual 5
  }
}
