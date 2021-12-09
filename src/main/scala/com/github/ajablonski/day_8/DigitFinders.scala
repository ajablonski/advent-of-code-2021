package com.github.ajablonski.day_8


object DigitFinders {
  private type DigitFunction = (Set[Set[String]], Map[Int, Set[String]]) => Map[Int, Set[String]]

  def findAll(digitCombinations: Set[Set[String]]): Map[Int, Set[String]] = {
    Seq(
      DigitFinders.find1,
      DigitFinders.find4,
      DigitFinders.find7,
      DigitFinders.find8,
      DigitFinders.find9,
      DigitFinders.find6,
      DigitFinders.find3,
      DigitFinders.find2,
      DigitFinders.find5,
      DigitFinders.find0
    ).foldLeft(Map[Int, Set[String]]())(
      (mapSoFar, nextDigitFunction) => nextDigitFunction(digitCombinations, mapSoFar)
    )
  }

  def find1: DigitFunction = (digitCombinations, knownDigits) => {
    knownDigits + (1 -> digitCombinations
      .find(_.size == 2)
      .get)
  }

  def find4: DigitFunction = (digitCombinations, knownDigits) => {
    knownDigits + (4 -> digitCombinations
      .find(_.size == 4)
      .get)
  }

  def find7: DigitFunction = (digitCombinations, knownDigits) => {
    knownDigits + (7 -> digitCombinations
      .find(_.size == 3)
      .get)
  }

  def find8: DigitFunction = (digitCombinations, knownDigits) => {
    knownDigits + (8 -> digitCombinations
      .find(_.size == 7)
      .get)
  }

  def find3: DigitFunction = (digitCombinations, knownDigits) => {
    val digit1Combination = knownDigits
      .getOrElse(1, find1(digitCombinations, knownDigits)(1))

    knownDigits + (3 -> digitCombinations
      .find(combination => combination.size == 5 && digit1Combination.subsetOf(combination))
      .get)
  }

  def find6: DigitFunction = (digitCombinations, knownDigits) => {
    val digit1Combination = knownDigits
      .getOrElse(1, find1(digitCombinations, knownDigits)(1))

    knownDigits + (6 -> digitCombinations
      .find(combination => combination.size == 6 && !digit1Combination.subsetOf(combination))
      .get)
  }

  def find9: DigitFunction = (digitCombinations, knownDigits) => {
    val digit4Combination = knownDigits
      .getOrElse(4, find4(digitCombinations, knownDigits)(4))

    knownDigits + (9 -> digitCombinations
      .find(combination => combination.size == 6 && digit4Combination.subsetOf(combination))
      .get)
  }

  def find2: DigitFunction = (digitCombinations, knownDigits) => {
    val digit4Combination = knownDigits
      .getOrElse(4, find4(digitCombinations, knownDigits)(4))

    knownDigits + (2 -> digitCombinations
      .find(combination => combination.size == 5 && combination.intersect(digit4Combination).size == 2)
      .get)
  }

  def find5: DigitFunction = (digitCombinations, knownDigits) => {
    val digit2Combination = knownDigits
      .getOrElse(2, find2(digitCombinations, knownDigits)(2))

    knownDigits + (5 -> digitCombinations
      .find(combination => combination.size == 5 && combination.intersect(digit2Combination).size == 3)
      .get)
  }

  def find0: DigitFunction = (digitCombinations, knownDigits) => {
    val digit6Combination = knownDigits
      .getOrElse(6, find6(digitCombinations, knownDigits)(6))
    val digit9Combination = knownDigits
      .getOrElse(9, find9(digitCombinations, knownDigits)(9))

    knownDigits + (0 -> digitCombinations
      .find(combination => combination.size == 6 && combination != digit9Combination && combination != digit6Combination)
      .get)
  }
}
