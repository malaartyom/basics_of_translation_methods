import scala.util.matching.Regex

case object Trivia {
  val NEWLINE: Regex = "\\r?\\n".r
  val COMMENT: Regex = """#[^\r\n]*""".r
  val WHITESPACE: Regex = "[ \\t]+".r
}
