import Keywords.{getHardKeyword, getSoftKeyword, isHardKeyword, isKeyword, isSoftKeyword}
import PrimitiveTokens.{isIdentifier, isNewLine, isTrivia}
import TokenType.*
import syspro.tm.lexer.{IdentifierToken, KeywordToken, Lexer, Token}
import LiteralTokens.*
import Runes.{RUNE_CHAR, RUNE_CHARACTER}
import IndentationProcessor._

import java.util
import Symbols.*


case class Tokenizer() extends Lexer {

  override def lex(s: String): java.util.List[Token] = {
    val tokens: Tokens = Tokens()
    val indents: IndentationProcessor= IndentationProcessor()
    var idx = 0
    var current_char: Char = '0'
    var next_char: Char = '0'


    while (idx < s.length) {
      current_char = s(idx)
      next_char = lookup(s, idx + 1).getOrElse('0')
      tokens.addChar(current_char)
      idx += 1
      if (isNewLine(tokens.sb)) {
        val nextString: String = extractNextString()
        if (nextString.isEmpty) {
          ???
        }
        else if (hasIndentation(nextString)) {
          indents.dropLevel()
        }
        else if (hasOnlyWhitespaces(nextString)) {
          ???
        }
        else if (isEndOfFile(s, nextString, idx)) {
          indents.dropLevel()
        }
        else {
          indents.countIndentation(nextString)
        }

      }
      else if (isLongSymbol(tokens.sb)) {
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
      else if (isRuneStart(tokens.sb)) {
        val extractedRuneInterior: String = extractRune(idx, s)
        idx += extractedRuneInterior.length + 1
        tokens.updateState()
        tokens.addString(extractedRuneInterior)
        if (!isRuneInterior(extractedRuneInterior)) {
          tokens.add(idx - 1, Bad)
        } else {
          tokens.add(idx - 1, RuneLiteral)
        }
        tokens.updateState()
      }
      else if (isStringStart(tokens.sb)) {
        val extractedStringInterior: String = extractString(idx, s)
        idx += extractedStringInterior.length + 1
        tokens.updateState()
        tokens.addString(extractedStringInterior)
        if (!isStringInterior(extractedStringInterior)) {
          tokens.add(idx, Bad)
        } else {
          tokens.add(idx, StringLiteral)
        }
        tokens.updateState()
      }
      else if (isKeyword(tokens.sb) && !isKeyword(tokens.sb + next_char) && !isIdentifier(tokens.sb + next_char) || idx == s.length) {
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

  private def lookup(s: String, i: Int): Option[Char] = {
    if (i < s.length) {
      return Some(s(i))
    } else {
      return None
    }
  }

  private def extractString(i: Int, s: String): String = {
    var idx: Int = i
    var extractedString = ""
    while (s(idx) != '"') {
      extractedString += s(idx)
      idx += 1
    }
    return extractedString
  }

  private def extractRune(i: Int, s: String): String = {
    var idx: Int = i
    var extractedRune = ""
    while (s(idx) != "'"(0)) {
      extractedRune += s(idx)
      idx += 1
    }
    return extractedRune
  }


}



