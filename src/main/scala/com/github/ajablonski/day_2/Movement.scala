package com.github.ajablonski.day_2

sealed trait Movement

case class Forward(change: Int) extends Movement

case class Down(change: Int) extends Movement

case class Up(change: Int) extends Movement