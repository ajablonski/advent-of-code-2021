package com.github.ajablonski.day_16

import scala.runtime.RichInt
import java.lang.Long as JLong

object BinaryStringExtensions {
  extension (s: String) {
    def hexToBinary: String = {
      s.split("").map(s => RichInt(Integer.parseInt(s, 16)).toBinaryString.padBinary(4)).mkString
    }

    def fromBinary: Long = {
      JLong.parseLong(s, 2)
    }

    def padBinary(characters: Int): String = {
      s.reverse.padTo(characters, '0').reverse
    }
  }
}
