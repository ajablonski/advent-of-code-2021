package com.github.ajablonski.day_10

type ChunkChar = ClosingChar | OpeningChar
type OpeningChar = '[' | '{' | '<' | '('
type ClosingChar = ']' | '}' | '>' | ')'

object Chunk {
  given stringToChunkChar: Conversion[String, List[ChunkChar]] =
    _.toCharArray.toList.filter(_.isInstanceOf[ChunkChar]).map(_.asInstanceOf[ChunkChar])
}

object ChunkExtensions {
  extension (oc: OpeningChar) {
    def getClosingChar: ClosingChar = {
      completionCharMap(oc)
    }
  }
  
  private val completionCharMap: Map[OpeningChar, ClosingChar] = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )
  
  extension (cc: ClosingChar) {
    def getCorruptionScore: Int = {
      corruptionScoreMap(cc)
    }
    
    def getCompletionScore: Int = {
      completionScoreMap(cc)
    }
  }
  
  private val corruptionScoreMap: Map[ClosingChar, Int] = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  private val completionScoreMap: Map[ClosingChar, Int] = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )
}

case class ExpectedButWas(expected: ClosingChar, was: ClosingChar)
