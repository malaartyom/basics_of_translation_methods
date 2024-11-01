import Identifier.IDENTIFIER
import LiteralTokens.isNull
import Trivia.{COMMENT, NEWLINE, TRIVIA}

case object PrimitiveTokens {
  
  def isTrivia(s: String): Boolean = TRIVIA.matches(s)

  def isIdentifier(s: String, next: String = ""): Boolean = if (isNull(next)) false else IDENTIFIER.matches(s + next)

  def isComment(s: String): Boolean = COMMENT.matches(s)

  def isNewLine(s: String): Boolean = NEWLINE.matches(s)

  def isEndOfFile(all_file: String, next_string: String, current_index: Int): Boolean = current_index + next_string.length >= all_file.length


}
