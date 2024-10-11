import scala.util.matching.Regex

case object Identifier {
  private val LETTER_CHARACTER: Regex = "[\\p{L}\\p{Nl}]".r
  private val ID_CONTINUE: Regex = (LETTER_CHARACTER.regex + "|" + "[\\p{Nd}\\p{Pc}\\p{Mn}\\p{Mc}\\p{Cf}]").r
  private val ID_START: Regex = (LETTER_CHARACTER.regex + "|" + "_").r
  val IDENTIFIER: Regex = (ID_START.regex + "(" + ID_CONTINUE.regex + ")*").r
}
