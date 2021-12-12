package com.github.ajablonski.day_12

case class Path private(private val nodes: Seq[String]) {
  val lastNode: String = nodes.head

  def addNode(node: String): Path = {
    Path(node +: nodes)
  }

  def visited(node: String): Boolean = {
    nodes.contains(node)
  }

  def hasRevisitedSmallCave(): Boolean = {
    nodes.filter(n => n.toLowerCase == n)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .exists(_._2 > 1)
  }

  override def toString: String = {
    nodes.reverse.mkString(",")
  }
}

object Path {
  def build(nodes: String*): Path = {
    new Path(nodes.reverse)
  }
}
