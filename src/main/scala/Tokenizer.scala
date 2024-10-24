import Keywords.{getHardKeyword, getSoftKeyword, isHardKeyword, isKeyword, isSoftKeyword}
import PrimitiveTokens.{isIdentifier, isTrivia}
import TokenType.*
import syspro.tm.lexer.{IdentifierToken, KeywordToken, Lexer, Token}
import LiteralTokens._

import java.util
import Symbols.*





case class Tokenizer() extends Lexer {

  override def lex(s: String): java.util.List[Token] = {
    val tokens: Tokens = Tokens()
    var idx = 0
    var current_char: Char = '0'
    var next_char: Char = '0'


    while (idx < s.length) {
//      println(idx)
//      println(tokens.tokens)
//      println(tokens.comment)
//      println(tokens.sb)

      current_char = s(idx)
      next_char = lookup(s, idx + 1)
      tokens.addChar(current_char)
      idx += 1
      if (isLongSymbol(tokens.sb)) {
        tokens.addChar(next_char)
        idx += 1
        tokens.add(idx - 1, Symbol)
        tokens.updateState()
      }
      else if (isShortSymbol(tokens.sb)) {
        tokens.add(idx - 1, Symbol)
        tokens.updateState()
      }
      else if (isBoolean(tokens.sb)) {
        tokens.add(idx - 1, BooleanLiteral)
        tokens.updateState()
      }
      else if (isInteger(tokens.sb) && !isInteger(tokens.sb + next_char)) {
        val suffix: String = s.slice(idx, idx + 3)
        if (isSuffix(suffix)) {
          tokens.addString(suffix)
          idx += 3
        }
        tokens.add(idx - 1, IntegerLiteral)
        tokens.updateState()
      }
      else if (tokens.isStringStarted()) {
        val extractedString: String = extractString(idx, s)
        idx += extractedString.length + 1
        tokens.updateState()
        tokens.addString(extractedString)
        tokens.add(idx, StringLiteral)
        tokens.updateState()
      }
      else if (isKeyword(tokens.sb) && !isKeyword(tokens.sb + next_char) && !isIdentifier(tokens.sb + next_char)|| idx == s.length) {
        if (isHardKeyword(tokens.sb)) {
          tokens.add(idx - 1, HardKeyword)
          tokens.updateState()
        } else if (isSoftKeyword(tokens.sb)) {
          tokens.add(idx - 1, SoftKeyword)
          tokens.updateState()
        }

      } else if (isIdentifier(tokens.sb) && !isHardKeyword(tokens.sb) && !isSoftKeyword(tokens.sb)) {
        if (!isIdentifier(tokens.sb + next_char)) {
          tokens.add(idx - 1, Identifier)
          tokens.updateState()
        }
      }
    }
    return tokens.tokens
  }

  private def lookup(s: String, i: Int): Char = {
    if (i < s.length) {
      return s(i)
    } else {
      return 'x'
    }
  }

  private def extractString(i: Int, s: String): String = {
    var idx: Int = i
    var extractedString = ""
    while (s(idx) != '"') {
      extractedString += s(idx)
      idx += 1
    }
    extractedString

  }


}



