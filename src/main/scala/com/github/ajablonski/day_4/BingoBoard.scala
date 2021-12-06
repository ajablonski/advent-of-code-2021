package com.github.ajablonski.day_4

case class Cell(value: Int, isMarked: Boolean)

case class BingoBoard(private val cells: Seq[Seq[Cell]]) {
  private val cellsAsColumns = cells
    .flatMap(row => row.zipWithIndex)
    .groupBy { case (cell, columnIndex) => columnIndex }
    .toSeq
    .sortBy { case (columnIndex, _) => columnIndex }
    .map { case (_, cellsAndIndices) =>
      cellsAndIndices.map { case (cell, _) => cell }
    }

  def mark(number: Int): BingoBoard = BingoBoard(
    cells
      .map(row => row.map(
        cell => if (cell.value == number) cell.copy(isMarked = true) else cell
      ))
  )

  def hasWon: Boolean = {
    cells
      .exists(row => row.forall(_.isMarked)) ||
      cellsAsColumns
        .exists(columns => columns.forall(_.isMarked))
  }

  def getUnmarkedValues: Set[Int] = {
    cells
      .flatMap(row => row.filterNot(_.isMarked).map(_.value))
      .toSet  
  }
}

object BingoBoard {
  def parseBoard(boardInput: String): BingoBoard = {
    BingoBoard(boardInput
      .split("\n")
      .toSeq
      .map(row => row
        .strip()
        .split("\\s+")
        .map(number => Cell(number.toInt, false))))
  }
}
