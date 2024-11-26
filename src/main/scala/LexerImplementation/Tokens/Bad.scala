package LexerImplementation.Tokens


import LexerImplementation.Tokens.Keywords.isKeyword
import LexerImplementation.Tokens.LiteralTokens.{isBoolean, isInteger, isRuneStart, isStringStart}
import LexerImplementation.Tokens.PrimitiveTokens.*
import LexerImplementation.Tokens.Symbols.isShortSymbol


case object Bad {
  def isBad(s: String): Boolean =
      s.nonEmpty &&
      !isKeyword(s) && !isIdentifier(s) && !isBoolean(s) && !isInteger(s) && 
      !isStringStart(s) && !isRuneStart(s) && !isNewLine(s) && !isTrivia(s) && 
      !isComment(s) && !isCarriageReturn(s) && !isShortSymbol(s) 
}
