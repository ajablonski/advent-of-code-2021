package com.github.ajablonski.day_11

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OctopusGridTest extends AnyFlatSpec with Matchers {
  "parse" should "parse the grid into a map" in {
    OctopusGrid.parse(Seq(
      "12345",
      "67890"
    )) shouldBe OctopusGrid(Map(
      (0, 0) -> 1, (0, 1) -> 2, (0, 2) -> 3, (0, 3) -> 4, (0, 4) -> 5,
      (1, 0) -> 6, (1, 1) -> 7, (1, 2) -> 8, (1, 3) -> 9, (1, 4) -> 0,
    ))
  }

  "step1" should "age the grid by one step" in {
    OctopusGrid.parse(Seq(
      "5483143223",
      "2745854711",
      "5264556173",
      "6141336146",
      "6357385478",
      "4167524645",
      "2176841721",
      "6882881134",
      "4846848554",
      "5283751526"
    )).step1() shouldBe(OctopusGrid.parse(Seq(
      "6594254334",
      "3856965822",
      "6375667284",
      "7252447257",
      "7468496589",
      "5278635756",
      "3287952832",
      "7993992245",
      "5957959665",
      "6394862637"
    )), 0)
  }

  it should "increment the flash count" in {
    OctopusGrid.parse(Seq(
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000090000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000"
    )).step1()._2 shouldBe 1
  }

  it should "handle increasing adjacent energy levels" in {
    OctopusGrid.parse(Seq(
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000090000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000"
    )).step1()._1 shouldBe OctopusGrid.parse(Seq(
      "1111111111",
      "1111111111",
      "1111111111",
      "1111222111",
      "1111202111",
      "1111222111",
      "1111111111",
      "1111111111",
      "1111111111",
      "1111111111"
    ))
  }

  it should "not update the octopus that just flashed" in {
    OctopusGrid.parse(Seq(
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000090000",
      "0000800000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000"
    )).step1()._1 shouldBe OctopusGrid.parse(Seq(
      "1111111111",
      "1111111111",
      "1111111111",
      "1111222111",
      "1112302111",
      "1112032111",
      "1112221111",
      "1111111111",
      "1111111111",
      "1111111111"
    ))
  }

  it should "handle complex scenario" in {
    OctopusGrid.parse(Seq(
      "6594254334",
      "3856965822",
      "6375667284",
      "7252447257",
      "7468496589",
      "5278635756",
      "3287952832",
      "7993992245",
      "5957959665",
      "6394862637"
    )).step1() shouldBe(OctopusGrid.parse(Seq(
      "8807476555",
      "5089087054",
      "8597889608",
      "8485769600",
      "8700908800",
      "6600088989",
      "6800005943",
      "0000007456",
      "9000000876",
      "8700006848"
    )), 35)
  }


  "stepN" should "age the grid by N steps" in {
    OctopusGrid.stepN(OctopusGrid.parse(Seq(
      "5483143223",
      "2745854711",
      "5264556173",
      "6141336146",
      "6357385478",
      "4167524645",
      "2176841721",
      "6882881134",
      "4846848554",
      "5283751526"
    )), 100) shouldBe(
      OctopusGrid.parse(Seq(
        "0397666866",
        "0749766918",
        "0053976933",
        "0004297822",
        "0004229892",
        "0053222877",
        "0532222966",
        "9322228966",
        "7922286866",
        "6789998766"
      )), 1656)
  }
  
  "allSynced" should "indicate when a grid has all octopuses synced" in {
    OctopusGrid.parse(Seq(
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000"
    )).allSynced() shouldBe true
  }
  
  it should "indicate when all octopuses are not synced" in {
    OctopusGrid.parse(Seq(
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000090000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000",
      "0000000000"
    )).allSynced() shouldBe false
  }
}
