package com.github.ajablonski


trait AocTestData(val day: Int) {
  val inputFile: String = {
    getClass.getResource(s"/samples/day$day.txt").getFile
  }
}
