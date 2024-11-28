package LexerImplementation.Processors

import LexerImplementation.Tokens.LiteralTokens.{getInt, getSuffix, hasSuffix, toInt}
import LexerImplementation.Tokens.PrimitiveTokens.*
import LexerImplementation.Tokens.{Keywords, LiteralTokens, Symbols, TokenType}
import LexerImplementation.Tokens.TokenType.{Bad, BooleanLiteral, Dedent, HardKeyword, Identifier, Indent, IntegerLiteral, RuneLiteral, SoftKeyword, StringLiteral, Symbol}
import syspro.tm.lexer.*

import java.util
import scala.collection.mutable

case class TokensProcessor(str: String = "") extends Extractor {
  var tokens = new util.ArrayList[Token]()
  var lastLineBreak = 0
  var trueEnd = 0
  var sb = ""
  var lastSize = 0

  private var leading_trivia_length = 0
  private var trailing_trivia_length = 0
  private var trivia: String = ""
  private var start = 0
  private var end = 0
  

  def dropStringBuilder(): Unit = sb = ""

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

  def addChar(str: String): Unit = {
    if (!isTrivia(str)) {
      sb += str
    }
    else if (isNewLine(str) || isComment(str) || isCarriageReturn(str)) {
      sb += str
      trivia += str
      leading_trivia_length += 1
    }
    if (isTrivia(str) && !isNewLine(str) && !isComment(str)) {
      trivia += str
      leading_trivia_length += 1
    }
  }


  def addString(s: String): Unit = {
    sb += s
  }

  def add(indents: mutable.Seq[Token]): Unit = {
    indents.foreach(x => tokens.add(x))
  }

  def flush(num: Int, size: Int, place: Int = trueEnd + 1, fileLength: Int): Unit = {
    var 
    i = num
    while (i > 0) {
      if (place + size <= fileLength - 1) {
        add(place, Dedent(size, place, place + size), flushFlag = true)
      }
      else {
        add(place, Dedent(0, place + 1, place + 1), flushFlag = true)
      }
      i-=1
    }
  }

  def add(idx: Int, tokenType: TokenType, flushFlag: Boolean = false): Unit = {
    val nextTrivia: String = extractTillEnd(idx + 1) // TODO : Fix it

    if (tokens.isEmpty) {
      leading_trivia_length = trivia.length
      start = 0
      end = idx
    }
    else {
      leading_trivia_length = trivia.length
      if (!IndentationProcessor.isSyntheticToken(tokens.getLast)) {
        start = end + 1
        end = idx
      } else {
        end = idx
        start = end - leading_trivia_length - sb.length + 1
      }
    }

    if (isEndOfFile(str, nextTrivia, idx + 1) && isTrivia(nextTrivia) && !isNewLine(nextTrivia)) {
      trailing_trivia_length = nextTrivia.length
      end = trailing_trivia_length + idx
      trivia = ""
    }
    if (!IndentationProcessor.isSyntheticToken(tokenType)) {
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
      case Indent(len, start, end) => this.end = this.end - 2; new IndentationToken(idx - toInt(!flushFlag) * 1, idx - toInt(!flushFlag) + len, 0, 0, 1)
      case Dedent(len, start, end) => this.end = this.end - 2; new IndentationToken(idx - toInt(!flushFlag) * 1, idx - toInt(!flushFlag) + len, 0, 0, -1)
      case IntegerLiteral =>
        val hasSuf: Boolean = hasSuffix(sb)
        val suffix: BuiltInType = getSuffix(sb, hasSuf)
        new IntegerLiteralToken(start, end, leading_trivia_length, trailing_trivia_length, suffix, hasSuf, getInt(sb, hasSuf))
    })
  }


  private def extractNextTrivia(idx: Int): String = extract(s = this.str, stop = "", idx = idx, function = (x, y) => (isTrivia(x) || isCarriageReturn(x)))

  private def extractTillEnd(idx: Int): String = {
    var i = idx
    var extractedString = ""
    var flag = true
    while (i < str.length && flag) {
      extractedString += str(i)
      if (isTrivia(extractedString) || isCarriageReturn(str(i).toString)) {
        flag = true
      } else flag = false
      i += 1

    }
    extractedString
  }

  override def extract(s: String, stop: String, idx: Int, function: (String, String) => Boolean): String = super.extract(s, stop, idx, function)


}
