import scala.util.matching.Regex

case object Runes {
  private val SHORT_ESCAPE:          Regex = """\\0|\\a|\\b|\\r|\\n|\\t|\\v|\\'|\\"|\\\\ """.r
  private val UNICODE_ESCAPE:        Regex = """\\u[0-9A-Fa-f]{4,5}""".r
  val ESCAPE:                        Regex = (SHORT_ESCAPE.regex + "|" + UNICODE_ESCAPE.regex).r
  private val SIMPLE_RUNE_CHARACTER: Regex = """[\\'\r\n]""".r
  private val RUNE_CHARACTER:        Regex = (SIMPLE_RUNE_CHARACTER.regex + "|" + ESCAPE).r
  val RUNE:                          Regex = ("\'" + RUNE_CHARACTER.regex + "\'").r
}
