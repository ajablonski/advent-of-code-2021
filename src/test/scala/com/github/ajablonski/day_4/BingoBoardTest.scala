package com.github.ajablonski.day_4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BingoBoardTest extends AnyFlatSpec with Matchers {
  private val testBoard = BingoBoard.parseBoard(
    """22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19""".stripMargin
  )

  "parse" should "return a board" in {
    BingoBoard.parseBoard(
      """22 13 17 11  0
        | 8  2 23  4 24
        |21  9 14 16  7
        | 6 10  3 18  5
        | 1 12 20 15 19""".stripMargin) shouldBe BingoBoard(Seq(
      Seq(Cell(22, false), Cell(13, false), Cell(17, false), Cell(11, false), Cell(0, false)),
      Seq(Cell(8, false), Cell(2, false), Cell(23, false), Cell(4, false), Cell(24, false)),
      Seq(Cell(21, false), Cell(9, false), Cell(14, false), Cell(16, false), Cell(7, false)),
      Seq(Cell(6, false), Cell(10, false), Cell(3, false), Cell(18, false), Cell(5, false)),
      Seq(Cell(1, false), Cell(12, false), Cell(20, false), Cell(15, false), Cell(19, false)),
    ))
  }

  "mark" should "return same board if number is not found" in {
    testBoard.mark(30) shouldBe testBoard
  }

  it should "return new board with number marked if number is found" in {
    testBoard.mark(0) shouldBe BingoBoard(Seq(
      Seq(Cell(22, false), Cell(13, false), Cell(17, false), Cell(11, false), Cell(0, true)),
      Seq(Cell(8, false), Cell(2, false), Cell(23, false), Cell(4, false), Cell(24, false)),
      Seq(Cell(21, false), Cell(9, false), Cell(14, false), Cell(16, false), Cell(7, false)),
      Seq(Cell(6, false), Cell(10, false), Cell(3, false), Cell(18, false), Cell(5, false)),
      Seq(Cell(1, false), Cell(12, false), Cell(20, false), Cell(15, false), Cell(19, false)),
    ))
  }

  "hasWon" should "return false given a board without a winning row" in {
    testBoard.hasWon shouldBe false
  }

  it should "return true when a row has won" in {
    testBoard
      .mark(22)
      .mark(13)
      .mark(17)
      .mark(11)
      .mark(0).hasWon shouldBe true
  }

  it should "return true when a column has won" in {
    testBoard
      .mark(11)
      .mark(4)
      .mark(16)
      .mark(18)
      .mark(15).hasWon shouldBe true
  }
  
  "getUnmarked" should "return empty list if all cells are marked" in {
    BingoBoard(Seq(
      Seq(Cell(22, true), Cell(13, true), Cell(17, true), Cell(11, true), Cell(0, true)),
      Seq(Cell(8, true), Cell(2, true), Cell(23, true), Cell(4, true), Cell(24, true)),
      Seq(Cell(21, true), Cell(9, true), Cell(14, true), Cell(16, true), Cell(7, true)),
      Seq(Cell(6, true), Cell(10, true), Cell(3, true), Cell(18, true), Cell(5, true)),
      Seq(Cell(1, true), Cell(12, true), Cell(20, true), Cell(15, true), Cell(19, true)),
    )).getUnmarkedValues shouldBe Set()
  }
  
  it should "return all cells that are marked" in {
    BingoBoard(Seq(
      Seq(Cell(22, true), Cell(13, false), Cell(17, true), Cell(11, true), Cell(0, true)),
      Seq(Cell(8, true), Cell(2, false), Cell(23, true), Cell(4, true), Cell(24, true)),
      Seq(Cell(21, true), Cell(9, false), Cell(14, true), Cell(16, true), Cell(7, true)),
      Seq(Cell(6, true), Cell(10, false), Cell(3, true), Cell(18, true), Cell(5, true)),
      Seq(Cell(1, true), Cell(12, false), Cell(20, true), Cell(15, true), Cell(19, true)),
    )).getUnmarkedValues shouldBe Set(13, 2, 9, 10, 12)
  }
}
