package com.github.ajablonski.day_17

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class TrenchStateTest extends AnyFunSpec with should.Matchers {
  describe("step") {
    it("should advance by one step") {
      TrenchState((0, 0), (5, 5), BoundaryArea(0, 1, 2, 3)).step() should be(
        TrenchState((5, 5), (4, 4), BoundaryArea(0, 1, 2, 3))
      )
    }

    it("should set the minimum x velocity to 0") {
      TrenchState((0, 0), (0, 5), BoundaryArea(0, 1, 2, 3)).step() should be(
        TrenchState((0, 5), (0, 4), BoundaryArea(0, 1, 2, 3))
      )
    }

    it("should handle negative x velocities") {
      TrenchState((0, 0), (-2, 5), BoundaryArea(0, 1, 2, 3)).step() should be(
        TrenchState((-2, 5), (-1, 4), BoundaryArea(0, 1, 2, 3))
      )
    }
  }

  describe("isPastBoundary") {
    it("should indicate when the current position is lower than the boundary and the y velocity is not positive") {
      TrenchState((0, 0), (0, 0), BoundaryArea(-2, 2, 1, 200)).isPastBoundary should be(true)
    }

    it("should indicate when the current position is lower than the boundary but the y velocity is positive") {
      TrenchState((0, 0), (0, 1), BoundaryArea(-2, 2, 1, 200)).isPastBoundary should be(false)
    }

    it("should indicate when the current position is above the boundary but the y velocity is not positive") {
      TrenchState((0, 201), (0, 0), BoundaryArea(-2, 2, 1, 200)).isPastBoundary should be(false)
    }

    it("should indicate when the current y position is in the boundary but the y velocity is not positive") {
      TrenchState((0, 200), (0, 0), BoundaryArea(-2, 2, 1, 200)).isPastBoundary should be(false)
      TrenchState((0, 1), (0, 0), BoundaryArea(-2, 2, 1, 200)).isPastBoundary should be(false)
    }

    it("should indicate when the current x position is too far to the right and the x velocity is not negative") {
      TrenchState((3, 0), (0, 5), BoundaryArea(-2, 2, 1, 200)).isPastBoundary should be(true)
      TrenchState((3, 0), (1, 5), BoundaryArea(-2, 2, 1, 200)).isPastBoundary should be(true)
    }

    it("should indicate when the current x position is too far to the left and the x velocity is not positive") {
      TrenchState((-3, 0), (0, 5), BoundaryArea(-2, 2, 1, 200)).isPastBoundary should be(true)
      TrenchState((-3, 0), (-1, 5), BoundaryArea(-2, 2, 1, 200)).isPastBoundary should be(true)
    }
  }

  describe("isInBoundary") {
    it("should return true when the current position is within the boundaries") {
      TrenchState((0, 0), (0, 0), BoundaryArea(-2, 2, -2, 2)).isInBoundary should be(true)
      TrenchState((-2, 0), (0, 0), BoundaryArea(-2, 2, -2, 2)).isInBoundary should be(true)
      TrenchState((2, 0), (0, 0), BoundaryArea(-2, 2, -2, 2)).isInBoundary should be(true)
      TrenchState((0, -2), (0, 0), BoundaryArea(-2, 2, -2, 2)).isInBoundary should be(true)
      TrenchState((0, 2), (0, 0), BoundaryArea(-2, 2, -2, 2)).isInBoundary should be(true)
    }

    it("should return false when the current position is not within the boundaries") {
      TrenchState((-3, -3), (0, 0), BoundaryArea(-2, 2, -2, 2)).isInBoundary should be(false)
      TrenchState((-3, 0), (0, 0), BoundaryArea(-2, 2, -2, 2)).isInBoundary should be(false)
      TrenchState((-3, 3), (0, 0), BoundaryArea(-2, 2, -2, 2)).isInBoundary should be(false)

      TrenchState((0, -3), (0, 0), BoundaryArea(-2, 2, -2, 2)).isInBoundary should be(false)
      TrenchState((0, 3), (0, 0), BoundaryArea(-2, 2, -2, 2)).isInBoundary should be(false)

      TrenchState((3, -3), (0, 0), BoundaryArea(-2, 2, -2, 2)).isInBoundary should be(false)
      TrenchState((3, 0), (0, 0), BoundaryArea(-2, 2, -2, 2)).isInBoundary should be(false)
      TrenchState((3, 3), (0, 0), BoundaryArea(-2, 2, -2, 2)).isInBoundary should be(false)
    }
  }

  describe("hasBeenInBoundary") {
    it("should be true if the location has ever been within the boundaries") {
      TrenchState((0, 0), (0, 0), BoundaryArea(1, 3, 1, 3)).hasBeenInBoundary should be(false)
      TrenchState((0, 0), (3, 1), BoundaryArea(1, 3, 1, 3)).step().hasBeenInBoundary should be(true)
      TrenchState((0, 0), (3, 1), BoundaryArea(1, 3, 1, 3)).step().step().hasBeenInBoundary should be(true)
    }

    it("should be false if the location has not ever been within the boundaries") {
      TrenchState((0, 0), (0, 0), BoundaryArea(1, 3, 1, 3)).hasBeenInBoundary should be(false)
      TrenchState((0, 0), (4, 1), BoundaryArea(1, 3, 1, 3)).step().hasBeenInBoundary should be(false)
    }
  }
}
