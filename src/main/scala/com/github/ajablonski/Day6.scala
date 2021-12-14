package com.github.ajablonski

object Day6 extends AocProblem[Long, Long] {
  override def part1(filename: String): Long = {
    agePopulationFromFile(filename, 80).sum
  }

  override def part2(filename: String): Long = {
    agePopulationFromFile(filename, 256).sum
  }

  private def agePopulationFromFile(filename: String, days: Int): Seq[Long] = {
    val fishAgeSeq = (0 to 8)
      .map(age => getRawData(filename)
        .head
        .split(",")
        .map(_.toInt)
        .count(_ == age)
        .toLong)
    agePopulation(days, fishAgeSeq)
  }

  def agePopulation(daysToAge: Int, fishAgeSeq: Seq[Long]): Seq[Long] = {
    (1 to daysToAge)
      .foldLeft(fishAgeSeq)((ages, day) => {
        val reproducingFish = ages.head
        val newAges = ages.slice(1, 7) :+ (ages(7) + reproducingFish) :+ ages(8) :+ reproducingFish
        newAges
      })
  }

}
