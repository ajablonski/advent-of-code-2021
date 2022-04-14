package com.github.ajablonski.day_17

case class TrenchState(currentPosition: (Int, Int), 
                       velocity: (Int, Int), 
                       boundaryArea: BoundaryArea, 
                       private val wasInBoundary: Boolean = false,
                       private val maxHeight: Int = 0) {

  def step(): TrenchState = {
    copy(
      currentPosition = (currentPosition._1 + velocity._1, currentPosition._2 + velocity._2),
      velocity = (velocity._1 match {
        case x if x > 0 => x - 1
        case x if x < 0 => x + 1
        case 0 => 0
      }, velocity._2 - 1),
      wasInBoundary = hasBeenInBoundary,
      maxHeight = getMaxHeight
    )
  }

  def isPastBoundary: Boolean = {
    (velocity._2 <= 0 && currentPosition._2 < boundaryArea.yMin) ||
      (velocity._1 >= 0 && currentPosition._1 > boundaryArea.xMax) ||
      (velocity._1 <= 0 && currentPosition._1 < boundaryArea.xMin)
  }

  def isInBoundary: Boolean = {
    (boundaryArea.xMin <= currentPosition._1 && currentPosition._1 <= boundaryArea.xMax) &&
      (boundaryArea.yMin <= currentPosition._2 && currentPosition._2 <= boundaryArea.yMax)
  }

  def hasBeenInBoundary: Boolean = wasInBoundary || isInBoundary
  
  def getMaxHeight: Int = math.max(maxHeight, currentPosition._2)
}
