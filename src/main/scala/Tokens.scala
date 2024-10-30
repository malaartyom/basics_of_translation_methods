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

  private var trivia: String = ""
  private var start = 0
  private var end = 0

  var sb = ""


  def dropStringBuilder(): Unit = sb = ""

  def updateState(): Unit = {
    sb = ""
    trailing_trivia_length = 0
    leading_trivia_length = 0
    trivia = ""
  }

  def addChar(char: Char): Unit = {
    if (!isTrivia(char.toString)) {
      sb += char
    }
    else if (isNewLine(char.toString) || isComment(char.toString)) {
      sb += char
      trivia += char
      trailing_trivia_length += 1
    }
    if (isTrivia(char.toString) && !isNewLine(char.toString)) {
      trivia += char
      trailing_trivia_length += 1
    }
  }

  def addString(s: String): Unit = {
    sb += s
  }

  def add(idx: Int, tokenType: TokenType, num: Int): Unit =
    val x = num.abs
    for (_ <- 1 to x) add(idx, tokenType)

  def add(idx: Int, tokenType: TokenType): Unit = {
    if (tokens.isEmpty) {
      trailing_trivia_length = 0
      leading_trivia_length = 0 // ???
      start = 0
      end = idx + leading_trivia_length
    } else {
      leading_trivia_length = 0
      trailing_trivia_length = trivia.length
      start = end + 1
      end = idx
    }
    tokens.add(tokenType match {
      case HardKeyword => new KeywordToken(start, end, leading_trivia_length, trailing_trivia_length, Keywords.getHardKeyword(sb))
      case SoftKeyword => new IdentifierToken(start, end, leading_trivia_length, trailing_trivia_length, sb, Keywords.getSoftKeyword(sb))
      case Identifier => new IdentifierToken(start, end, leading_trivia_length, trailing_trivia_length, sb, null)
      case Symbol => new SymbolToken(start, end, leading_trivia_length, trailing_trivia_length, Symbols.getSymbol(sb))
      case BooleanLiteral => new BooleanLiteralToken(start, end,leading_trivia_length, trailing_trivia_length, LiteralTokens.getBoolean(sb))
      case StringLiteral => new StringLiteralToken(start, end, leading_trivia_length, trailing_trivia_length, sb)
      case RuneLiteral => new RuneLiteralToken(start, end, leading_trivia_length, trailing_trivia_length, sb.codePointAt(0))
      case Bad => new BadToken(start, end, leading_trivia_length, trailing_trivia_length)
      case Indent => end = idx - 2; new IndentationToken(idx - 1, idx - 1, 0, 0, 1)
      case Dedent => end = idx - 2; new IndentationToken(idx - 1, idx - 1, 0, 0, -1)
      case IntegerLiteral =>
        val hasSuf: Boolean = hasSuffix(sb)
        val suffix: BuiltInType = getSuffix(sb, hasSuf)
        new IntegerLiteralToken(start, end, leading_trivia_length, trailing_trivia_length, suffix, hasSuf, getInt(sb, hasSuf))
    })
  }
}
