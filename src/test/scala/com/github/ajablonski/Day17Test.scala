package com.github.ajablonski

import com.github.ajablonski.day_17.BoundaryArea
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path}

class Day17Test extends AnyFunSpec with Matchers with BeforeAndAfterEach with AocTestData(17) {
  private val file: File = Files.createTempFile(this.getClass.getName, ".txt").toFile

  override protected def beforeEach(): Unit = {
    file.createNewFile()
  }

  override protected def afterEach(): Unit = {
    file.delete()
  }

  describe("part1") {
    it("should return highest possible height") {
      Day17.part1(inputFile) should be(45)
    }
  }

  describe("part2") {
    it("should return the number of distinct velocity combinations") {
      Day17.part2(inputFile) should be(112)
    }
  }

  describe("getParsedData") {
    it("should return the boundary area for positive coordinates") {
      new PrintWriter(file) {
        write("target area: x=70..96, y=100..124\n"); close()
      }
      Day17.getParsedData(file.getAbsolutePath) shouldEqual BoundaryArea(70, 96, 100, 124)
    }

    it("should return the boundary area for negative coordinates") {
      new PrintWriter(file) {
        write("target area: x=-100..-96, y=-100..-24\n"); close()
      }
      Day17.getParsedData(file.getAbsolutePath) shouldEqual BoundaryArea(-100, -96, -100, -24)
    }

    it("should return the boundary area for mixed coordinates") {
      new PrintWriter(file) {
        write("target area: x=-100..96, y=-100..24\n"); close()
      }
      Day17.getParsedData(file.getAbsolutePath) shouldEqual BoundaryArea(-100, 96, -100, 24)
    }
  }
}
