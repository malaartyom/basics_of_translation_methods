import PrimitiveTokens._
import TokenType._
import syspro.tm.lexer.{KeywordToken, IdentifierToken, BooleanLiteralToken, SymbolToken, Token, IntegerLiteralToken, BuiltInType, StringLiteralToken}
import java.util
import LiteralTokens._

case class Tokens() {
  var tokens = new util.ArrayList[Token]()
  private var start_idx = 0
  private var leading_trivia_length = 0
  private var trailing_trivia_length = 0
  var comment = ""
  private var commentStart = false
  private var stringStart = false
  var sb = ""

  def updateState(): Unit = {
    sb = ""
    // TODO: Think about trivia len
    trailing_trivia_length = leading_trivia_length
    leading_trivia_length = 0
    stringStart = false
    commentStart = false
  }

  def addChar(char: Char): Unit = {
    if (char == '"') {
      stringStart = true
    }
    if (isComment(comment) && !isComment(comment + char)) {
      commentStart = false
      trailing_trivia_length = comment.length
      comment = ""
    }
    else if (isComment(comment + char.toString)) {
      comment += char
      commentStart = true
    }
    if (!isTrivia(char.toString) && !commentStart) {
      sb += char
    } else {
      leading_trivia_length += 1
    }
  }

  def isStringStarted(): Boolean = stringStart

  def addString(s: String): Unit = {
    sb += s
  }

  def add(idx: Int, tokenType: TokenType): Unit = {
    tokens.add(tokenType match {
      case HardKeyword => new KeywordToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, Keywords.getHardKeyword(sb))
      case SoftKeyword => new IdentifierToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, sb, Keywords.getSoftKeyword(sb))
      case Identifier => new IdentifierToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, sb, null)
      case Symbol => new SymbolToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, Symbols.getSymbol(sb))
      case BooleanLiteral => new BooleanLiteralToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, LiteralTokens.getBoolean(sb))
      case StringLiteral => new StringLiteralToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, sb)
      case IntegerLiteral =>
        val hasSuf: Boolean = hasSuffix(sb)
        val suffix: BuiltInType = getSuffix(sb, hasSuf)
        new IntegerLiteralToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, suffix, hasSuf, getInt(sb, hasSuf))
    })
  }
}
