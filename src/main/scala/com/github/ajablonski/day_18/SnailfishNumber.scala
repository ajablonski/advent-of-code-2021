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

  def reduce(): SnailfishNumber = {
    findFirstNeedingExplode()
      .flatMap(_.explodeFromNode())
      .orElse {
        findFirstNeedingSplit()
          .map(_.splitFromNode())
      }
      .getOrElse(this)
  }

  def explodeFromNode(): Option[SnailfishNumber] = {
    val (leftVal, rightVal) = (left.asInstanceOf[Int], right.asInstanceOf[Int])
    getParent.get.addFromRightChild(leftVal, this)
      .findFirstNeedingExplode()
      .map(node => node.getParent.get.addFromLeftChild(rightVal, node))
      .flatMap(_.findFirstNeedingExplode())
      .map(_.replaceMeWith(0))
  }

  def splitFromNode(): SnailfishNumber = {
    this match {
      case SnailfishNumber(x: Int, _) if x > 9 => replaceMeWith(copy(left = SnailfishNumber(x / 2, (x + 1) / 2)))
      case SnailfishNumber(_, y: Int) if y > 9 => replaceMeWith(copy(right = SnailfishNumber(y / 2, (y + 1) / 2)))
      case SnailfishNumber(_, _) => throw Exception("Did not expect to receive SnailfishNumber needing splitting but with neither element an integer over 9")
    }
  }

  @tailrec
  final def reduced: SnailfishNumber = {
    val reducedOnce = this.reduce()
    if (reducedOnce == this) {
      this
    } else {
      reducedOnce.reduced
    }
  }

  def findFirstNeedingExplode() = findFirstAtDepth(MaxDepth + 1)

  def findFirstAtDepth(depth: Int): Option[SnailfishNumber] = {
    (left, right) match {
      case (_: Int, _: Int) => if (depth == 1) Some(this) else None
      case (x: SnailfishNumber, _: Int) => x.findFirstAtDepth(depth - 1)
      case (_: Int, y: SnailfishNumber) => y.findFirstAtDepth(depth - 1)
      case (x: SnailfishNumber, y: SnailfishNumber) => x.findFirstAtDepth(depth - 1).orElse(y.findFirstAtDepth(depth - 1))
    }
  }

  def findFirstNeedingSplit(): Option[SnailfishNumber] = {
    (left, right) match {
      case (x: Int, y: Int) => if (x > 9 || y > 9) Some(this) else None
      case (x: SnailfishNumber, y: Int) => x.findFirstNeedingSplit().orElse(if (y > 9) Some(this) else None)
      case (x: Int, y: SnailfishNumber) => if (x > 9) Some(this) else y.findFirstNeedingSplit()
      case (x: SnailfishNumber, y: SnailfishNumber) => x.findFirstNeedingSplit().orElse(y.findFirstNeedingSplit())
    }
  }

  def addFromRightChild(value: Int, child: SnailfishNumber): SnailfishNumber = {
    left match {
      case x: Int => this.replaceMeWith(copy(left = x + value))
      case l: SnailfishNumber if l == child && l.parentInfo == child.parentInfo =>
        this.getParent match {
          case Some(parent) => parent.addFromRightChild(value, this)
          case None => this
        }
      case l: SnailfishNumber => this.replaceMeWith(copy(left = l.addToRightChild(value)))
    }
  }

  def addFromLeftChild(value: Int, child: SnailfishNumber): SnailfishNumber = {
    right match {
      case x: Int => this.replaceMeWith(copy(right = x + value))
      case r: SnailfishNumber if r == child && r.parentInfo == child.parentInfo =>
        this.getParent match {
          case Some(parent) => parent.addFromLeftChild(value, this)
          case None => this
        }
      case r: SnailfishNumber => this.replaceMeWith(copy(right = r.addToLeftChild(value)))
    }
  }

  def addToLeftChild(value: Int): SnailfishNumber = {
    left match {
      case x: Int => copy(left = x + value)
      case l: SnailfishNumber => copy(left = l.addToLeftChild(value))
    }
  }

  def addToRightChild(value: Int): SnailfishNumber = {
    right match {
      case x: Int => copy(right = x + value)
      case r: SnailfishNumber => copy(right = r.addToRightChild(value))
    }
  }

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
}

object SnailfishNumber {
  val MaxDepth: Int = 4

  private def setParents(sfn: SnailfishNumber): Unit = {

    sfn.left match {
      case _: Int =>
      case x: SnailfishNumber => {
        x.parentInfo = Some((sfn, true))
        setParents(x)
      }
    }

    sfn.right match {
      case _: Int =>
      case x: SnailfishNumber => {
        x.parentInfo = Some((sfn, false))
        setParents(x)
      }
    }
  }
}

