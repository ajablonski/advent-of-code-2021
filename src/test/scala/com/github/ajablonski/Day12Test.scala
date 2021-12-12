package com.github.ajablonski

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.ajablonski.day_12.{Cave, Connection, Path}



class Day12Test extends AnyFlatSpec with Matchers with AocTestData(12) {

  "part1" should "return total number of paths" in {
    Day12.part1(inputFile) shouldBe 10
  }

  "part2" should "return total number of paths allowing visiting one small node twice" in {
    Day12.part2(inputFile) shouldBe 36
  }

  "buildAdjacencyGraph" should "build map containing adjacent nodes" in {
    import Cave.stringToCave

    Day12.buildAdjacencyMap(Seq(
      Connection("start", "A"),
      Connection("A", "b"),
      Connection("b", "end")
    )) shouldBe Map(
      Cave("start") -> Set(Cave("A")),
      Cave("A") -> Set(Cave("start"), Cave("b")),
      Cave("b") -> Set(Cave("A"), Cave("end")),
      Cave("end") -> Set(Cave("b"))
    )
  }

  "findPaths" should "return all paths" in {
    Day12.findPaths(Seq(
      Connection("start", "A"),
      Connection("A", "b"),
      Connection("b", "end")
    )) shouldBe Set(
      Path.build("start", "A", "b", "end")
    )
  }

  it should "find multiple paths if available without visiting small caves more than once" in {
    Day12.findPaths(Seq(
      Connection("start", "A"),
      Connection("start", "b"),
      Connection("A", "b"),
      Connection("b", "end"),
      Connection("A", "end")
    )) shouldBe Set(
        Path.build("start", "b", "A", "end"),
        Path.build("start", "A", "end"),
        Path.build("start", "A", "b", "A", "end"),
        Path.build("start", "b", "end"),
        Path.build("start", "A", "b", "end")
    )
  }

  "parse" should "return connections" in {
    Day12.parse(Seq(
      "start-A",
        "start-b",
        "A-c",
        "A-b",
        "b-d",
        "A-end",
        "b-end"
    )) shouldBe Seq(
     Connection("start", "A"),
     Connection("start", "b"),
     Connection("A", "c"),
     Connection("A", "b"),
     Connection("b", "d"),
     Connection("A", "end"),
     Connection("b", "end")
    )
  }

}
