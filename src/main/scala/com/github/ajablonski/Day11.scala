package com.github.ajablonski

import com.github.ajablonski.day_11.OctopusGrid

object Day11 extends AocProblem[Int] {
  override def part1(filename: String): Int = {
    
    OctopusGrid.stepN(OctopusGrid.parse(getRawData(filename)), 100)._2
      
      
  }

  override def part2(filename: String): Int = {
    
    OctopusGrid.findSyncedStep(OctopusGrid.parse(getRawData(filename)))
  }
}
