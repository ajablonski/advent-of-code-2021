package com.github.ajablonski.day_12

case class Cave(name: String) {
  def isSmall: Boolean = name.toLowerCase == name
}

object Cave {
  given stringToCave: Conversion[String, Cave] = {
    Cave(_)
  }
}
