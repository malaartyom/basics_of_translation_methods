package LexerImplementation.Tokens

import scala.util.matching.Regex

object Runes {
  val RUNE_CHAR = "'"
  val SHORT_ESCAPE = """\\[0abfnrtv'\\]"""
  val UNICODE_ESCAPE = """\\u[0-9a-fA-F]{4}|\\U\+[0-9a-fA-F]{4,5}"""
  val ESCAPE = s"($SHORT_ESCAPE|$UNICODE_ESCAPE)"
  val SIMPLE_RUNE_CHARACTER = """[^'\\\r\n]"""
  val RUNE_CHARACTER = s"($SIMPLE_RUNE_CHARACTER|$ESCAPE)"
  val RUNE_INTERIOR: Regex = RUNE_CHARACTER.r
  val RUNE: Regex = s"$RUNE_CHAR$RUNE_CHARACTER$RUNE_CHAR".r
}
