package com.github.ajablonski.day_8

import com.github.ajablonski.Day8
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DigitFindersTest extends AnyFlatSpec with Matchers {

  private val digitCombinations = Day8
    .parseLine("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe")
    ._1

  "find1" should "find the 2-segment digit" in {
    DigitFinders.find1(digitCombinations, Map()) shouldBe Map(1 -> Set("b", "e"))
  }

  "find4" should "find the 4-segment digit" in {
    DigitFinders.find4(digitCombinations, Map()) shouldBe Map(4 -> Set("b", "c", "e", "g"))
  }

  "find7" should "find the 3-segment digit" in {
    DigitFinders.find7(digitCombinations, Map()) shouldBe Map(7 -> Set("b", "e", "d"))
  }

  "find8" should "find the 7-segment digit" in {
    DigitFinders.find8(digitCombinations, Map()) shouldBe Map(8 -> Set("a", "b", "c", "d", "e", "f", "g"))
  }

  "find3" should "find the 5-segment value which includes all the segments of 1" in {
    DigitFinders.find3(digitCombinations, Map(
      1 -> Set("b", "e")
    )) shouldBe Map(
      1 -> Set("b", "e"),
      3 -> Set("b", "c", "d", "e", "f")
    )
  }

  it should "work when no previous mapping is provided" in {
    DigitFinders.find3(digitCombinations, Map()) shouldBe Map(
      3 -> Set("b", "c", "d", "e", "f")
    )
  }

  "find6" should "find the 6-segment number for which 1 isn't a subset" in {
    DigitFinders.find6(digitCombinations, Map(
      1 -> Set("b", "e")
    )) shouldBe Map(
      1 -> Set("b", "e"),
      6 -> Set("a", "c", "d", "e", "f", "g")
    )
  }

  it should "work when no previous mapping is provided" in {
    DigitFinders.find6(digitCombinations, Map()) shouldBe Map(
      6 -> Set("a", "c", "d", "e", "f", "g")
    )
  }

  "find9" should "find the 6-segment number for which 4 is a subset" in {
    DigitFinders.find9(digitCombinations, Map(4 -> Set("b", "c", "e", "g"))) shouldBe Map(
      4 -> Set("b", "c", "e", "g"),
      9 -> Set("b", "c", "d", "e", "f", "g")
    )
  }

  it should "work when no previous mapping is provided" in {
    DigitFinders.find9(digitCombinations, Map()) shouldBe Map(
      9 -> Set("b", "c", "d", "e", "f", "g")
    )
  }

  "find2" should "find the 5-segment value whose intersection with 4 has size 2" in {
    DigitFinders.find2(digitCombinations, Map(4 -> Set("b", "c", "e", "g"))) shouldBe Map(
      4 -> Set("b", "c", "e", "g"),
      2 -> Set("a", "b", "c", "d", "f")
    )
  }

  it should "work when no previous mapping is provided" in {
    DigitFinders.find2(digitCombinations, Map()) shouldBe Map(
      2 -> Set("a", "b", "c", "d", "f")
    )
  }

  "find5" should "find the 5-segment value whose intersection with 2 has size 3" in {
    DigitFinders.find5(digitCombinations, Map(2 -> Set("a", "b", "c", "d", "f"))) shouldBe Map(
      5 -> Set("c", "d", "e", "f", "g"),
      2 -> Set("a", "b", "c", "d", "f")
    )
  }

  it should "work when no previous mapping is provided" in {
    DigitFinders.find5(digitCombinations, Map()) shouldBe Map(
      5 -> Set("c", "d", "e", "f", "g")
    )
  }

  "find0" should "return the 6-segment number that isn't 6 or 9" in {
    DigitFinders.find0(digitCombinations, Map(
      6 -> Set("a", "c", "d", "e", "f", "g"),
      9 -> Set("b", "c", "d", "e", "f", "g")
    )) shouldBe Map(
      0 -> Set("a", "b", "d", "e", "f", "g"),
      9 -> Set("b", "c", "d", "e", "f", "g"),
      6 -> Set("a", "c", "d", "e", "f", "g")
    )
  }

  it should "work when no previous mapping is provided" in {
    DigitFinders.find0(digitCombinations, Map()) shouldBe Map(
      0 -> Set("a", "b", "d", "e", "f", "g"),
    )
  }

  "findAll" should "return mapping of numbers to segment sets" in {
    DigitFinders.findAll(digitCombinations) shouldBe Map(
      0 -> Set("e", "f", "a", "b", "g", "d"),
      5 -> Set("e", "f", "g", "c", "d"),
      1 -> Set("b", "e"),
      6 -> Set("e", "f", "a", "g", "c", "d"),
      9 -> Set("e", "f", "b", "g", "c", "d"),
      2 -> Set("f", "a", "b", "c", "d"),
      7 -> Set("e", "d", "b"),
      3 -> Set("e", "f", "b", "c", "d"),
      8 -> Set("e", "f", "a", "b", "g", "c", "d"),
      4 -> Set("c", "g", "e", "b"))
  }
}
