package MyLexer.Tokens


import MyLexer.Tokens.Keywords.{isHardKeyword, isKeyword, isSoftKeyword}
import MyLexer.Tokens.LiteralTokens.{isBoolean, isInteger, isRuneInterior, isRuneStart, isStringInterior, isStringStart, isSuffix, notNull}
import MyLexer.Tokens.PrimitiveTokens.{isCarriageReturn, isComment, isIdentifier, isLongNewLine, isNewLine, isTrivia}
import MyLexer.Tokens.Symbols.{isLongSymbol, isShortSymbol}


case object Bad {
  def isBad(s: String): Boolean =
      s.nonEmpty &&
      !isKeyword(s) && !isIdentifier(s) && !isBoolean(s) && !isInteger(s) && 
      !isStringStart(s) && !isRuneStart(s) && !isNewLine(s) && !isTrivia(s) && 
      !isComment(s) && !isCarriageReturn(s) && !isShortSymbol(s) 
}
