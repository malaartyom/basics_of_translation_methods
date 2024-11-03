package MyLexer.Tokens

import scala.util.matching.Regex

case object Trivia {
  val NEWLINE: Regex = """\r?\n""".r
  val COMMENT: Regex = """#[^\r\n]*""".r
  val WHITESPACE: Regex = """[ \t]+""".r
  val CARRIAGE_RETURN: Regex = """\r""".r

  private val NEWLINE_STR: String = "\\r?\\n"
  private val COMMENT_STR: String = """#[^\r\n]*"""
  private val WHITESPACE_STR: String = "[ \\t]+"

  val TRIVIA: Regex = s"($NEWLINE_STR|$COMMENT_STR|$WHITESPACE_STR)+".r

}
