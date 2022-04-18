package com.github.ajablonski.day_18

import com.github.ajablonski.day_18.SnailfishNumber.{setParent, setParents}

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

  def depth: Int = {
    (left, right) match {
      case (_: Int, _: Int) => 1
      case (x: SnailfishNumber, _: Int) => x.depth + 1
      case (_: Int, y: SnailfishNumber) => y.depth + 1
      case (x: SnailfishNumber, y: SnailfishNumber) => math.max(x.depth, y.depth) + 1
    }
  }

  def reduce(): SnailfishNumber = {
    if (depth > 4) {
      val afterLeftAddition = findFirstAtDepth(5)
        .map(node => node.getParent.get.addFromRightChild(node.left.asInstanceOf[Int], node))
      val afterRightAddition = afterLeftAddition
        .flatMap(_.findFirstAtDepth(5))
        .map(node => node.getParent.get.addFromLeftChild(node.right.asInstanceOf[Int], node))
      val afterReplacement = afterRightAddition
        .flatMap(_.findFirstAtDepth(5))
        .map(_.replaceMeWith(0))
      afterReplacement
        .get
    } else if (findFirstNeedingSplit().isDefined) {
      findFirstNeedingSplit()
        .map {
          case node@SnailfishNumber(x: Int, right) if x > 9 => node.replaceMeWith(SnailfishNumber(left = SnailfishNumber(x / 2, (x + 1) / 2), right = right))
          case node@SnailfishNumber(left, y: Int) if y > 9 => node.replaceMeWith(SnailfishNumber(left = left, right = SnailfishNumber(y / 2, (y + 1) / 2)))
          case SnailfishNumber(_, _) => throw Exception("Did not expect to receive SnailfishNumber needing splitting but with neither element an integer over 9")
        }
        .get
    } else {
      this
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
      case x: Int => this.replaceMeWith(SnailfishNumber(left = x + value, right = right))
      case l: SnailfishNumber if l != child => this.replaceMeWith(SnailfishNumber(left = l.addToRightChild(value), right = right))
      case l: SnailfishNumber if l == child =>
        this.getParent match {
          case Some(parent) => parent.addFromRightChild(value, this)
          case None => this
        }
    }
  }

  def addFromLeftChild(value: Int, child: SnailfishNumber): SnailfishNumber = {
    right match {
      case x: Int => this.replaceMeWith(SnailfishNumber(left = left, right = x + value))
      case r: SnailfishNumber if r == child && r.parentInfo == child.parentInfo =>
        this.getParent match {
          case Some(parent) => parent.addFromLeftChild(value, this)
          case None => this
        }
      case r: SnailfishNumber => this.replaceMeWith(SnailfishNumber(left = left, right = r.addToLeftChild(value)))
    }
  }

  def addToLeftChild(value: Int): SnailfishNumber = {
    this match {
      case SnailfishNumber(x: Int, _) => SnailfishNumber(left = x + value, right = right)
      case SnailfishNumber(l: SnailfishNumber, _) => SnailfishNumber(left = l.addToLeftChild(value), right = right)
    }
  }

  def addToRightChild(value: Int): SnailfishNumber = {
    this match {
      case SnailfishNumber(_, x: Int) => SnailfishNumber(left = left, right = x + value)
      case SnailfishNumber(_, r: SnailfishNumber) => SnailfishNumber(left = left, right = r.addToRightChild(value))
    }
  }

  @tailrec
  final def replaceMeWith(value: Int | SnailfishNumber): SnailfishNumber = {
    parentInfo match {
      case Some((parent, true)) => parent.replaceMeWith(SnailfishNumber(left = value, right = parent.right))
      case Some((parent, false)) => parent.replaceMeWith(SnailfishNumber(left = parent.left, right = value))
      case None =>
        value match {
          case x: SnailfishNumber =>
            val returnValue = x.copy()
            setParents(returnValue)
            returnValue
          case _: Int =>
            throw Exception("Attempting to replace a root Snailfish node with an integer")
        }
    }
  }
}

object SnailfishNumber {
  private def setParent(item: Int | SnailfishNumber, parent: SnailfishNumber, isLeftChild: Boolean): Unit = {
    item match {
      case _: Int =>
      case x: SnailfishNumber =>
        x.parentInfo = Some((parent, isLeftChild))
    }
  }

  private def setParents(sfn: SnailfishNumber): Unit = {
    setParent(sfn.left, sfn, true)
    setParent(sfn.right, sfn, false)

    sfn.left match {
      case _: Int =>
      case x: SnailfishNumber => setParents(x)
    }

    sfn.right match {
      case _: Int =>
      case x: SnailfishNumber => setParents(x)
    }
  }
}

