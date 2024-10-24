import Runes.ESCAPE

import scala.util.matching.Regex

case object StringLiteral {
  private val STRING_CHAR = "\""
  private val SIMPLE_STRING_CHARACTER = """[^"\\\r\n]"""
  private val STRING_CHARACTER  = s"($SIMPLE_STRING_CHARACTER|$ESCAPE)"
  val STRING: Regex = s"$STRING_CHAR$STRING_CHARACTER*$STRING_CHAR".r
}
