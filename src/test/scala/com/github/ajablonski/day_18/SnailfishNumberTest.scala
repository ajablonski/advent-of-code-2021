package com.github.ajablonski.day_18

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should
import com.github.ajablonski.day_18.{SnailfishNumber => SNF}

class SnailfishNumberTest extends AnyFunSpec with should.Matchers {

  describe("parent") {
    it("should be correctly initialized") {
      val snf = SNF(SNF(1, 2), 3)
      snf.left.asInstanceOf[SNF].getParent shouldBe Some(snf)
      snf.getParent shouldBe None
    }
  }

  describe("replaceMeWith") {
    it("should correctly replace value") {
      val snf = SNF(SNF(1, 2), 3)

      snf.left.asInstanceOf[SNF].replaceMeWith(0) shouldBe SNF(0, 3)
    }

    it("should work given deep nesting") {
      val snf = SNF(SNF(SNF(SNF(SNF(1, 2), 3), 4), 5), 6)

      snf
        .left.asInstanceOf[SNF]
        .left.asInstanceOf[SNF]
        .left.asInstanceOf[SNF]
        .left.asInstanceOf[SNF]
        .replaceMeWith(0) shouldBe SNF(SNF(SNF(SNF(0, 3), 4), 5), 6)
    }
  }

  describe("magnitude") {
    it("should return the magnitude as 3 times the left quantity plus 2 times the right quantity") {
      SNF(1, 9).magnitude should be(21)
      SNF(9, 1).magnitude should be(29)
    }

    it("should work recursively") {
      SNF(SNF(9, 1), SNF(1, 9)).magnitude should be(129)
    }
  }

  describe("add") {
    it("should work for numbers nested less than 3 layers") {
      SNF(SNF(1, 2), SNF(3, 4)) + SNF(SNF(5, 6), SNF(7, 8)) shouldBe SNF(SNF(SNF(1, 2), SNF(3, 4)), SNF(SNF(5, 6), SNF(7, 8)))
    }

    it("should handle more complex case") {
      SNF(SNF(SNF(0, SNF(4, 5)), SNF(0, 0)), SNF(SNF(SNF(4, 5), SNF(2, 6)), SNF(9, 5))) +
        SNF(7, SNF(SNF(SNF(3, 7), SNF(4, 3)), SNF(SNF(6, 3), SNF(8, 8)))) shouldBe
        SNF(SNF(SNF(SNF(4, 0), SNF(5, 4)), SNF(SNF(7, 7), SNF(6, 0))), SNF(SNF(8, SNF(7, 7)), SNF(SNF(7, 9), SNF(5, 0))))
    }

    it("should handle another complex case") {
      SNF(SNF(SNF(SNF(6,7),SNF(6,7)),SNF(SNF(7,7),SNF(0,7))),SNF(SNF(SNF(8,7),SNF(7,7)),SNF(SNF(8,8),SNF(8,0))))
      + SNF(SNF(SNF(SNF(2,4),7),SNF(6,SNF(0,5))),SNF(SNF(SNF(6,8),SNF(2,8)),SNF(SNF(2,1),SNF(4,5)))) shouldBe
        SNF(SNF(SNF(SNF(7,0),SNF(7,7)),SNF(SNF(7,7),SNF(7,8))),SNF(SNF(SNF(7,7),SNF(8,8)),SNF(SNF(7,7),SNF(8,7))))
    }
  }

  describe("depth") {
    it("should return the maximum depth of the number") {
      SNF(1, 2).depth shouldBe 1
      SNF(SNF(1, 2), 1).depth shouldBe 2
      SNF(1, SNF(1, 2)).depth shouldBe 2
      SNF(SNF(1, 2), SNF(1, 2)).depth shouldBe 2
      SNF(SNF(SNF(1, 2), 2), SNF(1, 2)).depth shouldBe 3
    }
  }

  describe("reduce") {
    it("should return the same number if already reduced") {
      val original = SNF(SNF(SNF(SNF(2, 1), 2), 3), 4)
      original.reduce() shouldBe original
    }

    it("should reduce when only an explode is needed and there are no numbers to the left") {
      SNF(SNF(SNF(SNF(SNF(9, 8), 1), 2), 3), 4).reduce() shouldBe SNF(SNF(SNF(SNF(0, 9), 2), 3), 4)
    }

    it("should reduce when only an explode is needed and there are no numbers to the right") {
      SNF(7, SNF(6, SNF(5, SNF(4, SNF(3, 2))))).reduce() shouldBe SNF(7, SNF(6, SNF(5, SNF(7, 0))))
    }

    it("should reduce when only an explode is needed and there are numbers on both sides") {
      SNF(SNF(6, SNF(5, SNF(4, SNF(3, 2)))), 1).reduce() shouldBe SNF(SNF(6, SNF(5, SNF(7, 0))), 3)
    }

    it("should reduce even when that leads to new deep elements") {
      SNF(SNF(3, SNF(2, SNF(1, SNF(7, 3)))), SNF(6, SNF(5, SNF(4, SNF(3, 2))))).reduce() shouldBe SNF(SNF(3, SNF(2, SNF(8, 0))), SNF(9, SNF(5, SNF(4, SNF(3, 2)))))
    }

    it("should split when that's the only requirement") {
      SNF(10, 11).reduce() shouldBe SNF(SNF(5, 5), 11)
    }

    it("should explode first before splitting") {
      SNF(SNF(SNF(SNF(SNF(20, 22), 1), 2), 3), 4).reduce() shouldBe SNF(SNF(SNF(SNF(0, 23), 2), 3), 4)
    }

    it("should handle when nested values are equivalent") {
      SNF(SNF(SNF(SNF(SNF(6, 7), SNF(6, 7)), 2), 3), 4).reduce() shouldBe SNF(SNF(SNF(SNF(0, SNF(13, 7)), 2), 3), 4)
    }
  }

  describe("reduced") {
    it("should return the fully reduced result of a value with only explode requirements") {
      SNF(SNF(3, SNF(2, SNF(1, SNF(7, 3)))), SNF(6, SNF(5, SNF(4, SNF(3, 2))))).reduced shouldBe SNF(SNF(3, SNF(2, SNF(8, 0))), SNF(9, SNF(5, SNF(7, 0))))
    }

    it("should return the fully reduced result of a value with only split requirements") {
      SNF(10, 11).reduced shouldBe SNF(SNF(5, 5), SNF(5, 6))
    }
  }
}
