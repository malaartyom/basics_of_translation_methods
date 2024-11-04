package MyLexer.Processors

import MyLexer.Tokens.Keywords
import MyLexer.Tokens.LiteralTokens
import MyLexer.Tokens.LiteralTokens.{hasSuffix, getSuffix, getInt, toInt}
import MyLexer.Tokens.PrimitiveTokens.*
import MyLexer.Tokens.Symbols
import MyLexer.Tokens.TokenType.{Symbol, HardKeyword,SoftKeyword,
Indent,Dedent, Identifier, Bad, RuneLiteral, StringLiteral, BooleanLiteral, IntegerLiteral}
import MyLexer.Tokens.TokenType
import scala.collection.mutable
import MyLexer.Tokens.TokenType
import syspro.tm.lexer.*


import java.util

case class TokensProcessor(str: String = "") extends Extractor {
  var tokens = new util.ArrayList[Token]()
  var lastLineBreak = 0
  var trueEnd = 0
  var sb = ""

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

  def flush(num: Int, stack: mutable.Stack[Token], place: Int = trueEnd + 1, length: Int): Unit = {
    var 
    i = num
    while (i > 0) {
      val indent = stack.pop()
      val len = indent.end - indent.start
      if (place + len <= length - 1) {
        add(place, Dedent(len, place, place + len), flushFlag = true)
      }
      else {
        add(place, Dedent(0, place + 1, place + 1), flushFlag = true)
      }
      i-=1
    }
  }


//  def add(idx: Int, tokenType: TokenType, num: Int, flushFlag: Boolean): Unit =
//    val x = num.abs
//    for (_ <- 1 to x) add(idx, tokenType, flushFlag)


  def add(idx: Int, tokenType: TokenType, flushFlag: Boolean = false): Unit = {
    val nextTrivia: String = extractTillEnd(idx + 1)

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

//  override def extract(s: String, stop: String, idx: Int, function: (String, String) => Boolean): String =
//    super.extract(s = this.str, stop = "", idx = idx, function)  // Slow

  private def extractTillEnd(idx: Int): String = {
    var i = idx
    var extractedString = ""
    while (i < str.length) { // TODO: Fix (using extract from ?tokenizer?)
      extractedString += str(i)
      i += 1
    }
    extractedString
  }


}
