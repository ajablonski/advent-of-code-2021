package com.github.ajablonski.day_4

import scala.annotation.tailrec

case class BingoGame(numbersToCall: Seq[Int], boards: Set[BingoBoard], lastDrawn: Option[Int] = None) {
  def draw(): BingoGame = BingoGame(
    numbersToCall.tail,
    boards.map(_.mark(numbersToCall.head)),
    Option(numbersToCall.head)
  )

  val winningBoard: Option[BingoBoard] = boards.find(_.hasWon)

  val hasWon: Boolean = winningBoard.isDefined
}

object BingoGame {
  def parse(lines: Seq[String]): BingoGame = {
    lines.mkString("\n").split("\n\n").toSeq match {
      case Seq(numbersToCall, boardList: _*) => BingoGame(
        numbersToCall.split(",").map(_.toInt),
        boardList.map(BingoBoard.parseBoard).toSet
      )
    }
  }

  @tailrec
  def play(game: BingoGame): BingoGame = {
    if (game.hasWon) game else play(game.draw())
  }

  @tailrec
  def playToLastBoard(game: BingoGame): BingoGame = {
    if (game.hasWon && game.boards.size == 1) {
      game
    } else if (game.hasWon) {
      playToLastBoard(game.copy(boards = game.boards.filterNot(_.hasWon)).draw())
    } else {
      playToLastBoard(game.draw())
    }
  }
}
