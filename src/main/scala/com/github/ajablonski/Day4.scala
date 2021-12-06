package com.github.ajablonski

import com.github.ajablonski.day_4.BingoGame
import com.github.ajablonski.day_4.BingoGame.{play, playToLastBoard}

object Day4 extends AocProblem {
  override def part1(filename: String): Int = {
    val finishedBingoGame = play(BingoGame.parse(getRawData(filename)))
    (for (
      winningBoard <- finishedBingoGame.winningBoard;
      lastNumber <- finishedBingoGame.lastDrawn
    ) yield winningBoard.getUnmarkedValues.sum * lastNumber).get
  }

  override def part2(filename: String): Int = {
    val finishedBingoGame = playToLastBoard(BingoGame.parse(getRawData(filename)))
    (for (
      winningBoard <- finishedBingoGame.winningBoard;
      lastNumber <- finishedBingoGame.lastDrawn
    ) yield winningBoard.getUnmarkedValues.sum * lastNumber).get

  }
}
