package MyLexer.Tokens


import MyLexer.Tokens.Keywords.isKeyword
import MyLexer.Tokens.LiteralTokens.{isBoolean, isInteger, isRuneStart, isStringStart}
import MyLexer.Tokens.PrimitiveTokens.*
import MyLexer.Tokens.Symbols.isShortSymbol


case object Bad {
  def isBad(s: String): Boolean =
      s.nonEmpty &&
      !isKeyword(s) && !isIdentifier(s) && !isBoolean(s) && !isInteger(s) && 
      !isStringStart(s) && !isRuneStart(s) && !isNewLine(s) && !isTrivia(s) && 
      !isComment(s) && !isCarriageReturn(s) && !isShortSymbol(s) 
}
