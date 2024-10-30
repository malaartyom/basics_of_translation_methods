import PrimitiveTokens._
import TokenType._
import syspro.tm.lexer.{KeywordToken, IdentifierToken,
  BooleanLiteralToken, SymbolToken, Token, IntegerLiteralToken,
  BuiltInType, StringLiteralToken, RuneLiteralToken, BadToken, IndentationToken}
import java.util
import LiteralTokens._

case class Tokens() {
  var tokens = new util.ArrayList[Token]()
  private var leading_trivia_length = 0
  private var trailing_trivia_length = 0

  var sb = ""


  def updateState(): Unit = {
    // TODO: DO some fixes in update State
    sb = ""
    // TODO: Think about trivia len
    trailing_trivia_length = leading_trivia_length
    leading_trivia_length = 0
  }

  def addChar(char: Char): Unit = {
    if (!isTrivia(char.toString)) {
      sb += char
    } else if (isNewLine(char.toString) || isComment(char.toString)) {
      sb += char
    }
    else {
      leading_trivia_length += 1
    }
  }

  def addString(s: String): Unit = {
    sb += s
  }

  def add(idx: Int, tokenType: TokenType, num: Int): Unit =
    val x = num.abs
    for (_ <- 1 to x) add(idx, tokenType)

  def add(idx: Int, tokenType: TokenType): Unit = {
    tokens.add(tokenType match {
      case HardKeyword => new KeywordToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, Keywords.getHardKeyword(sb))
      case SoftKeyword => new IdentifierToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, sb, Keywords.getSoftKeyword(sb))
      case Identifier => new IdentifierToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, sb, null)
      case Symbol => new SymbolToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, Symbols.getSymbol(sb))
      case BooleanLiteral => new BooleanLiteralToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, LiteralTokens.getBoolean(sb))
      case StringLiteral => new StringLiteralToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, sb)
      case RuneLiteral => new RuneLiteralToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, sb.codePointAt(0))
      case Bad => new BadToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length)
      case Indent => new IndentationToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, 1)
      case Dedent => new IndentationToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, -1)
      case IntegerLiteral =>
        val hasSuf: Boolean = hasSuffix(sb)
        val suffix: BuiltInType = getSuffix(sb, hasSuf)
        new IntegerLiteralToken(idx - sb.length + 1, idx, leading_trivia_length, trailing_trivia_length, suffix, hasSuf, getInt(sb, hasSuf))
    })
  }
}
