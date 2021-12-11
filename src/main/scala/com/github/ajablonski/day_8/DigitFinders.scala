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
    val digit1Combination = retrieveDigit(1, digitCombinations, knownDigits)

    knownDigits + (3 -> digitCombinations
      .find(combination => combination.size == 5 && digit1Combination.subsetOf(combination))
      .get)
  }

  def find6: DigitFunction = (digitCombinations, knownDigits) => {
    val digit1Combination = retrieveDigit(1, digitCombinations, knownDigits)

    knownDigits + (6 -> digitCombinations
      .find(combination => combination.size == 6 && !digit1Combination.subsetOf(combination))
      .get)
  }

  def find9: DigitFunction = (digitCombinations, knownDigits) => {
    val digit4Combination = retrieveDigit(4, digitCombinations, knownDigits)

    knownDigits + (9 -> digitCombinations
      .find(combination => combination.size == 6 && digit4Combination.subsetOf(combination))
      .get)
  }

  def find2: DigitFunction = (digitCombinations, knownDigits) => {
    val digit4Combination = retrieveDigit(4, digitCombinations, knownDigits)

    knownDigits + (2 -> digitCombinations
      .find(combination => combination.size == 5 && combination.intersect(digit4Combination).size == 2)
      .get)
  }

  def find5: DigitFunction = (digitCombinations, knownDigits) => {
    val digit2Combination = retrieveDigit(2, digitCombinations, knownDigits)

    knownDigits + (5 -> digitCombinations
      .find(combination => combination.size == 5 && combination.intersect(digit2Combination).size == 3)
      .get)
  }

  def find0: DigitFunction = (digitCombinations, knownDigits) => {
    val digit6Combination = retrieveDigit(6, digitCombinations, knownDigits)
    val digit9Combination = retrieveDigit(9, digitCombinations, knownDigits)

    knownDigits + (0 -> digitCombinations
      .find(combination => combination.size == 6 && combination != digit9Combination && combination != digit6Combination)
      .get)
  }

  private val digitFunctionMap: Map[Int, DigitFunction] = Map(
    0 -> find0,
    1 -> find1,
    2 -> find2,
    3 -> find3,
    4 -> find4,
    5 -> find5,
    6 -> find6,
    7 -> find7,
    8 -> find8,
    9 -> find9
  )

  private def retrieveDigit(digit: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9,
                            digitCombinations: Set[Set[String]],
                            knownDigits: Map[Int, Set[String]]): Set[String] = {
    val method = getClass
      .getMethod(s"find$digit")
    val instance = getClass.getDeclaredConstructor().newInstance()
    knownDigits
      .getOrElse(digit, digitFunctionMap(digit)(digitCombinations, knownDigits)(digit))
  }
}
