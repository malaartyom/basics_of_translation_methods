package LexerImplementation.Tokens

import Identifier.IDENTIFIER
import LexerImplementation.Tokens.Trivia.{CARRIAGE_RETURN, COMMENT, NEWLINE, TRIVIA}
import LiteralTokens.isNull

case object PrimitiveTokens {
  
  def isTrivia(s: String): Boolean = TRIVIA.matches(s)

  def isIdentifier(s: String, next: String = ""): Boolean = if (isNull(next)) false else IDENTIFIER.matches(s + next)

  def isComment(s: String): Boolean = COMMENT.matches(s)

  def isNewLine(s: String): Boolean = NEWLINE.matches(s)

  def isEndOfFile(all_file: String, next_string: String, current_index: Int): Boolean = current_index + next_string.length >= all_file.length
  
  def isCarriageReturn(s: String): Boolean = CARRIAGE_RETURN.matches(s)

  def isLongNewLine(s:String, next_char: String): Boolean =
    if (isNull(next_char)) return false   
    isCarriageReturn(s) && isNewLine(s + next_char)


}
