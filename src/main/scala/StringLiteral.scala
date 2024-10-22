import Runes.ESCAPE

import scala.util.matching.Regex

case object StringLiteral {
  private val SIMPLE_STRING_CHARACTER: Regex = """["\r\n]""".r
  private val STRING_CHARACTER: Regex = (SIMPLE_STRING_CHARACTER.regex + "|" + ESCAPE).r
  val STRING: Regex = ('"' + STRING_CHARACTER.regex + '"').r
}
