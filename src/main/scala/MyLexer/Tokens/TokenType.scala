package MyLexer.Tokens

import syspro.tm.lexer.{IndentationToken, Token}

enum TokenType {
  case Bad
  case BooleanLiteral
  case Identifier
  case Indent
  case Dedent
  case IntegerLiteral
  case HardKeyword
  case SoftKeyword
  case RuneLiteral
  case StringLiteral
  case Symbol


  def isSyntheticToken(token: Token): Boolean = token.isInstanceOf[IndentationToken]

  def isSyntheticToken(token: TokenType): Boolean = token == Indent || token == Dedent
}




