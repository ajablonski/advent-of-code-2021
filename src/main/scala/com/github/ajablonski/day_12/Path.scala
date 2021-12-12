package com.github.ajablonski.day_12

case class Path private(private val nodes: Seq[Cave]) {
  val lastNode: Cave = nodes.head

  def addNode(node: Cave): Path = {
    Path(node +: nodes)
  }

  def visited(node: Cave): Boolean = {
    nodes.contains(node)
  }

  def hasRevisitedSmallCave: Boolean = {
    nodes.filter(_.isSmall)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .exists(_._2 > 1)
  }

  override def toString: String = {
    nodes.reverse.mkString(",")
  }
}

object Path {
  def build(nodes: Cave*): Path = {
    new Path(nodes.reverse)
  }
}
