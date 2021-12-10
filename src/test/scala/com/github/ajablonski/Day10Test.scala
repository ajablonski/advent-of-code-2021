package com.github.ajablonski

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Test extends AnyFlatSpec with Matchers with AocTestData(10) {
  import Chunk.stringToChunkChar

  "part1" should "return total syntax error score" in {
    Day10.part1(inputFile) shouldBe 26397
  }

  "part2" should "return middle score of autocompletes" in {
    Day10.part2(inputFile) shouldBe 288957
  }

  "findUnexpectedClosings" should "return incorrect closing characters" in {
    Day10.findUnexpectedClosings("{([(<{}[<>[]}>{[]{[(<()>") shouldBe Some(ExpectedButWas(']', '}'))
    Day10.findUnexpectedClosings("[[<[([]))<([[{}[[()]]]") shouldBe Some(ExpectedButWas(']', ')'))
    Day10.findUnexpectedClosings("[{[{({}]{}}([{[{{{}}([]") shouldBe Some(ExpectedButWas(')', ']'))
    Day10.findUnexpectedClosings("[<(<(<(<{}))><([]([]()") shouldBe Some(ExpectedButWas('>', ')'))
    Day10.findUnexpectedClosings("<{([([[(<>()){}]>(<<{{") shouldBe Some(ExpectedButWas(']', '>'))
    Day10.findUnexpectedClosings("[({(<(())[]>[[{[]{<()<>>") shouldBe None
  }

  "scoreCorruptions" should "score corruptions based on the incorrect character" in {
    Day10.scoreCorruption(ExpectedButWas(']', ')')) shouldBe 3
    Day10.scoreCorruption(ExpectedButWas('}', ']')) shouldBe 57
    Day10.scoreCorruption(ExpectedButWas(']', '}')) shouldBe 1197
    Day10.scoreCorruption(ExpectedButWas(']', '>')) shouldBe 25137
  }

  "completeOrError" should "return the characters needed to complete the line" in {
    Day10.completeOrError("[({(<(())[]>[[{[]{<()<>>").map(_.mkString) shouldBe Right("}}]])})]")
    Day10.completeOrError("[(()[<>])]({[<{<<[]>>(").map(_.mkString) shouldBe Right(")}>]})")
    Day10.completeOrError("(((({<>}<{<{<>}{[]{[]{}").map(_.mkString) shouldBe Right("}}>}>))))")
    Day10.completeOrError("{<[[]]>}<{[{[{[]{()[[[]").map(_.mkString) shouldBe Right("]]}}]}]}>")
    Day10.completeOrError("<{([{{}}[<[[[<>{}]]]>[]]").map(_.mkString) shouldBe Right("])}>")
  }

  "scoreCompletion" should "score completion" in {
    Day10.scoreCompletion(List(')')) shouldBe 1
    Day10.scoreCompletion(List(']')) shouldBe 2
    Day10.scoreCompletion(List('}')) shouldBe 3
    Day10.scoreCompletion(List('>')) shouldBe 4
  }

  it should "multiply previous score by 5 before adding for multi-character completions" in {
    Day10.scoreCompletion("}}]])})]".toCharArray.toList.asInstanceOf[List[ClosingChar]]) shouldBe 288957
    Day10.scoreCompletion(")}>]})".toCharArray.toList.asInstanceOf[List[ClosingChar]]) shouldBe 5566
    Day10.scoreCompletion("}}>}>))))".toCharArray.toList.asInstanceOf[List[ClosingChar]]) shouldBe 1480781
    Day10.scoreCompletion("]]}}]}]}>".toCharArray.toList.asInstanceOf[List[ClosingChar]]) shouldBe 995444
    Day10.scoreCompletion("])}>".toCharArray.toList.asInstanceOf[List[ClosingChar]]) shouldBe 294
  }
}
