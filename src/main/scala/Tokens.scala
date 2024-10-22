import TokenType._
import syspro.tm.lexer.{IdentifierToken, KeywordToken, Token}

import java.util

case class Tokens() {
  private var tokens = new util.ArrayList[Token]()
  private var start_idx = 0
  private var leading_trivia_length = 0
  private var trailing_trivia_length = 0
  var sb = ""

  def updateState(): Unit = {
    this.sb = ""
    trailing_trivia_length = leading_trivia_length
    leading_trivia_length = 0
  }

  def addChar(char: String): Unit = {
    if (!isTrivia(char)) {
      sb += s(idx)
    } else {
      leading_trivia_length += 1
    }
  }

  def add(idx: Int, tokenType: TokenType): Unit = {
    tokens.add(tokenType match {
      case HardKeyword => KeywordToken(start_idx, idx, leading_trivia_length, trailing_trivia_length, Keywords.getHardKeyword(sb))
      case SoftKeyword => IdentifierToken(start_idx, idx, leading_trivia_length, trailing_trivia_length, sb, Keywords.getSoftKeyword(sb))

    })
  }
}
