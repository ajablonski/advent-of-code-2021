package com.github.ajablonski.day_13

case class FoldInstruction(axis: XorY, value: Int)

sealed trait XorY
case object X extends XorY
case object Y extends XorY
