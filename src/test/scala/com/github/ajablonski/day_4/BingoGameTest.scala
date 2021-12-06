package com.github.ajablonski.day_4

import com.github.ajablonski.AocTestData
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source
import org.scalatest.matchers.should.Matchers

import scala.util.Using

class BingoGameTest extends AnyFlatSpec with Matchers with AocTestData(4) {
  private val inputBoard = BingoBoard.parseBoard(
    """22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19
      |""".stripMargin)


  "parse" should "return a Bingo Game" in {
    val rawData = Using(Source.fromFile(inputFile)) { reader =>
      reader.getLines().toSeq
    }.get

    BingoGame.parse(rawData) shouldBe BingoGame(
      Seq(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1),
      Set(
        inputBoard,
        BingoBoard.parseBoard(
          """ 3 15  0  2 22
            | 9 18 13 17  5
            |19  8  7 25 23
            |20 11 10 24  4
            |14 21 16 12  6
            |""".stripMargin),
        BingoBoard.parseBoard(
          """14 21 17 24  4
            |10 16 15  9 19
            |18  8 23 26 20
            |22 11 13  6  5
            | 2  0 12  3  7
            |""".stripMargin)
      )
    )
  }

  "draw" should "advance the game one round" in {
    BingoGame(
      Seq(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1),
      Set(
        inputBoard))
      .draw() shouldBe BingoGame(
      Seq(4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1),
      Set(
        inputBoard.mark(7)),
        Some(7))
  }


  "getLastNumberDrawn" should "return previously drawn number" in {
    BingoGame(
      Seq(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1),
      Set(
        inputBoard))
    .draw()
    .lastDrawn shouldBe Some(7)
  }

  it should "return None when no numbers have been drawn before" in {
    BingoGame(
      Seq(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1),
      Set(
        inputBoard)).lastDrawn shouldBe None
  }

  "hasWon" should "return true when a card is complete" in {
    BingoGame(
      Seq(22,8,21,6,1,0),
      Set(
        inputBoard))
      .draw()
      .draw()
      .draw()
      .draw()
      .draw().hasWon shouldBe true
  }

  it should "return false when no board has won yet" in {
    BingoGame(
      Seq(22,8,21,6,1,0),
      Set(
        inputBoard))
      .draw()
      .draw()
      .draw()
      .draw().hasWon shouldBe false
  }

  "getWinningBoard" should "return None when no board has won yet" in {
    val inputBoard = BingoBoard.parseBoard(
      """22 13 17 11  0
        | 8  2 23  4 24
        |21  9 14 16  7
        | 6 10  3 18  5
        | 1 12 20 15 19
        |""".stripMargin)
    BingoGame(
      Seq(22,8,21,6,1,0),
      Set(inputBoard))
      .draw()
      .draw()
      .draw()
      .draw().winningBoard shouldBe None
  }

  it should "return winning board when a board has won" in {
    BingoGame(
      Seq(22,8,21,6,1,0),
      Set(inputBoard))
      .draw()
      .draw()
      .draw()
      .draw()
      .draw().winningBoard shouldBe Some(inputBoard.mark(22).mark(8).mark(21).mark(6).mark(1))

  }

  "play" should "advance game until a board has won" in {
    BingoGame.play(BingoGame(
      Seq(22,8,21,6,1,0),
      Set(inputBoard))) shouldBe BingoGame(Seq(0), Set(inputBoard.mark(22).mark(8).mark(21).mark(6).mark(1)), Some(1))
  }
}
