import Keywords.{getHardKeyword, getSoftKeyword, isHardKeyword, isSoftKeyword}
import PrimitiveTokens.{isIdentifier, isTrivia}
import TokenType._
import syspro.tm.lexer.{IdentifierToken, KeywordToken, Lexer, Token}

import java.util


case class Tokenizer() extends Lexer {

  override def lex(s: String): java.util.List[Token] = {
    val tokens: Tokens = Tokens()
    var idx = 0
    var current_char = ""



    while (idx < s.length) {
      current_char = s(idx).toString
      tokens.addChar(current_char)
      if (isTrivia(current_char) || idx == s.length) {
        if (isHardKeyword(tokens.sb)) {
          tokens.add(idx, HardKeyword)
          tokens.updateState()
        } else if (isSoftKeyword(tokens.sb)) {
          tokens.add(idx, SoftKeyword)
        }
      }
      idx += 1
    }
    return tokens
  }


}



