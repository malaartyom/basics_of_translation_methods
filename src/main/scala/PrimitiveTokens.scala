import Identifier.IDENTIFIER
import Trivia.{COMMENT, NEWLINE, WHITESPACE}

case object PrimitiveTokens {
  
  def isTrivia(s: String): Boolean = NEWLINE.matches(s) || COMMENT.matches(s) || WHITESPACE.matches(s)

  def isIdentifier(s: String): Boolean = IDENTIFIER.matches(s)

  def isComment(s: String): Boolean = COMMENT.matches(s)

  def isNewLine(s: String): Boolean = NEWLINE.matches(s)

}
