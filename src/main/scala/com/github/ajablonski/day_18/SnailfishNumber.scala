package com.github.ajablonski.day_18

import com.github.ajablonski.day_18.SnailfishNumber.{MaxDepth, setParents}

import scala.annotation.{tailrec, targetName}

case class SnailfishNumber(left: SnailfishNumber | Int, right: SnailfishNumber | Int) {
  private var parentInfo: Option[(SnailfishNumber, Boolean)] = None

  setParents(this)

  override def toString: String = f"[$left,$right]"

  def getParent: Option[SnailfishNumber] = parentInfo.map(_._1)

  def magnitude: Int = {
    3 * (left match {
      case x: Int => x
      case x: SnailfishNumber => x.magnitude
    })
      + 2 * (right match {
      case x: Int => x
      case x: SnailfishNumber => x.magnitude
    })
  }

  @targetName("plus")
  def +(other: SnailfishNumber): SnailfishNumber = {
    SnailfishNumber(this, other).reduced
  }

  /**
   * @return The fully reduced Snailfish number
   */
  @tailrec
  final def reduced: SnailfishNumber = {
    val reducedOnce = this.reduce()
    if (reducedOnce == this) {
      this
    } else {
      reducedOnce.reduced
    }
  }

  /**
   * @return The Snailfish number after performing one reduction step (explode or split)
   */
  def reduce(): SnailfishNumber = {
    findFirstChildNeedingExplode()
      .flatMap(_.explodeFromNode())
      .orElse {
        findFirstChildNeedingSplit()
          .map(_.splitFromNode())
      }
      .getOrElse(this)
  }

  /**
   * @return The full Snailfish number after exploding the current node
   */
  def explodeFromNode(): Option[SnailfishNumber] = {
    val (leftVal, rightVal) = (left.asInstanceOf[Int], right.asInstanceOf[Int])
    getParent.get.addFromRightChild(leftVal, this)
      .findFirstChildNeedingExplode()
      .map(node => node.getParent.get.addFromLeftChild(rightVal, node))
      .flatMap(_.findFirstChildNeedingExplode())
      .map(_.replaceMeWith(0))
  }

  /**
   * @return The full Snailfish number after splitting the current node
   */
  def splitFromNode(): SnailfishNumber = {
    this match {
      case SnailfishNumber(x: Int, _) if x > 9 => replaceMeWith(copy(left = SnailfishNumber(x / 2, (x + 1) / 2)))
      case SnailfishNumber(_, y: Int) if y > 9 => replaceMeWith(copy(right = SnailfishNumber(y / 2, (y + 1) / 2)))
      case SnailfishNumber(_, _) => throw Exception("Did not expect to receive SnailfishNumber needing splitting but with neither element an integer over 9")
    }
  }


  /**
   * @param value: The value which which to replace the current node
   * @return The full Snailfish number with the current node replaced with `value`
   */
  @tailrec
  final def replaceMeWith(value: Int | SnailfishNumber): SnailfishNumber = {
    parentInfo match {
      case Some((parent, true)) => parent.replaceMeWith(parent.copy(left = value))
      case Some((parent, false)) => parent.replaceMeWith(parent.copy(right = value))
      case None =>
        value match {
          case x: SnailfishNumber => x
          case _: Int =>
            throw Exception("Attempting to replace a root Snailfish node with an integer")
        }
    }
  }

  def findFirstChildNeedingExplode(): Option[SnailfishNumber] = findFirstAtDepth(MaxDepth + 1)

  def findFirstAtDepth(depth: Int): Option[SnailfishNumber] = this match {
    case SnailfishNumber(_: Int, _: Int) => if (depth == 1) Some(this) else None
    case SnailfishNumber(x: SnailfishNumber, _: Int) => x.findFirstAtDepth(depth - 1)
    case SnailfishNumber(_: Int, y: SnailfishNumber) => y.findFirstAtDepth(depth - 1)
    case SnailfishNumber(x: SnailfishNumber, y: SnailfishNumber) => x.findFirstAtDepth(depth - 1).orElse(y.findFirstAtDepth(depth - 1))
  }

  def findFirstChildNeedingSplit(): Option[SnailfishNumber] = this match {
    case SnailfishNumber(x: Int, y: Int) => if (x > 9 || y > 9) Some(this) else None
    case SnailfishNumber(x: Int, y: SnailfishNumber) => if (x > 9) Some(this) else y.findFirstChildNeedingSplit()
    case SnailfishNumber(x: SnailfishNumber, y: Int) => x.findFirstChildNeedingSplit().orElse(if (y > 9) Some(this) else None)
    case SnailfishNumber(x: SnailfishNumber, y: SnailfishNumber) => x.findFirstChildNeedingSplit().orElse(y.findFirstChildNeedingSplit())
  }

  @tailrec
  private def addFromRightChild(value: Int, child: SnailfishNumber): SnailfishNumber = {
    left match {
      case x: Int => this.replaceMeWith(copy(left = x + value))
      case l: SnailfishNumber if l.parentInfo == child.parentInfo =>
        this.getParent match {
          case Some(parent) => parent.addFromRightChild(value, this)
          case None => this
        }
      case l: SnailfishNumber => this.replaceMeWith(copy(left = l.addToRightChild(value)))
    }
  }

  @tailrec
  private def addFromLeftChild(value: Int, child: SnailfishNumber): SnailfishNumber = {
    right match {
      case x: Int => this.replaceMeWith(copy(right = x + value))
      case r: SnailfishNumber if r.parentInfo == child.parentInfo =>
        this.getParent match {
          case Some(parent) => parent.addFromLeftChild(value, this)
          case None => this
        }
      case r: SnailfishNumber => this.replaceMeWith(copy(right = r.addToLeftChild(value)))
    }
  }

  private def addToLeftChild(value: Int): SnailfishNumber = {
    left match {
      case x: Int => copy(left = x + value)
      case l: SnailfishNumber => copy(left = l.addToLeftChild(value))
    }
  }

  private def addToRightChild(value: Int): SnailfishNumber = {
    right match {
      case x: Int => copy(right = x + value)
      case r: SnailfishNumber => copy(right = r.addToRightChild(value))
    }
  }
}

object SnailfishNumber {
  val MaxDepth: Int = 4

  private def setParents(sfn: SnailfishNumber): Unit = {

    sfn.left match {
      case _: Int =>
      case x: SnailfishNumber =>
        x.parentInfo = Some((sfn, true))
        setParents(x)
    }

    sfn.right match {
      case _: Int =>
      case x: SnailfishNumber =>
        x.parentInfo = Some((sfn, false))
        setParents(x)
    }
  }
}

