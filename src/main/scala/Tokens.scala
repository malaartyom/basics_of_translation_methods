import PrimitiveTokens._
import TokenType._
import syspro.tm.lexer.{
  KeywordToken, IdentifierToken,
  BooleanLiteralToken, SymbolToken, Token, IntegerLiteralToken,
  BuiltInType, StringLiteralToken, RuneLiteralToken, BadToken, IndentationToken
}
import java.util
import LiteralTokens._
import PrimitiveTokens.isEndOfFile

case class Tokens(str: String = "") {
  var tokens = new util.ArrayList[Token]()
  var dedentsToFlush = 0
  var lastLineBreak = 0
  private var leading_trivia_length = 0
  private var trailing_trivia_length = 0

  private var trivia: String = ""
  private var start = 0
  private var end = 0
  private var trueEnd = 0

  var sb = ""


  def dropStringBuilder(): Unit = sb  = ""

  def updateState(): Unit = {
    sb = ""
    leading_trivia_length = 0
    trailing_trivia_length = 0
    trivia = ""
  }
  def addToTrivia(s: String): Unit = {
    leading_trivia_length += s.length
    trivia += s
  }
  def addChar(char: Char): Unit = {
    if (!isTrivia(char.toString)) {
      sb += char
    }
    else if (isNewLine(char.toString) || isComment(char.toString)) {
      sb += char
      trivia += char
      leading_trivia_length += 1
    }
    if (isTrivia(char.toString) && !isNewLine(char.toString) && !isComment(char.toString)) {
      trivia += char
      leading_trivia_length += 1
    }
  }

  def addString(s: String): Unit = {
    sb += s
  }

  def flush(i: Int): Unit =
    var idx = 0
    if (!tokens.isEmpty) {               // TODO: Причесать, выглядит ужасно 
      if (!isSyntheticToken(tokens.getLast)) {
        if (lastLineBreak == - 1) {
          idx = tokens.getLast.end + 1 
        }
        else {
          idx = lastLineBreak
        }
      }
      else {
        if (lastLineBreak == -1) {
          
        }
        
        idx = trueEnd + 1
      }
    }
    add(idx + 1, Dedent, dedentsToFlush)

  def add(idx: Int, tokenType: TokenType, num: Int): Unit =
    val x = num.abs
    for (_ <- 1 to x) add(idx, tokenType)


  def add(idx: Int, tokenType: TokenType): Unit = {
    val nextTrivia: String = extractTillEnd(idx + 1)

    if (tokens.isEmpty) {
      leading_trivia_length = 0
      start = 0
    }
    else {
      leading_trivia_length = trivia.length
      if (!isSyntheticToken(tokens.getLast)) {
        start = end + 1
      }
    }

    end = idx
    if (isEndOfFile(str, nextTrivia, idx + 1) && isTrivia(nextTrivia) && !(isNewLine(nextTrivia))) {
      trailing_trivia_length = nextTrivia.length
      end = trailing_trivia_length + idx
      trivia = ""
    }
    if (!isSyntheticToken(tokenType)) {
      trueEnd = end 
    }
    tokens.add(tokenType match {
      case HardKeyword => new KeywordToken(start, end, leading_trivia_length, trailing_trivia_length, Keywords.getHardKeyword(sb))
      case SoftKeyword => new IdentifierToken(start, end, leading_trivia_length, trailing_trivia_length, sb, Keywords.getSoftKeyword(sb))
      case Identifier => new IdentifierToken(start, end, leading_trivia_length, trailing_trivia_length, sb, null)
      case Symbol => new SymbolToken(start, end, leading_trivia_length, trailing_trivia_length, Symbols.getSymbol(sb))
      case BooleanLiteral => new BooleanLiteralToken(start, end, leading_trivia_length, trailing_trivia_length, LiteralTokens.getBoolean(sb))
      case StringLiteral => end = end - 1; new StringLiteralToken(start, end, leading_trivia_length, trailing_trivia_length, sb)
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
  private def extractTillEnd(idx: Int): String = {
    var i = idx
    var extractedString = ""
    while (i < str.length) {               // TODO: Fix (using extract from ?tokenizer?) 
      extractedString += str(i)
      i += 1
    }
    return extractedString
  }
  private def isSyntheticToken(token: Token): Boolean = token.isInstanceOf[IndentationToken]

  private def isSyntheticToken(token: TokenType): Boolean = token == Indent || token == Dedent

}
