import Keywords.{isHardKeyword, isKeyword, isSoftKeyword}
import PrimitiveTokens.{isComment, isIdentifier, isNewLine}
import TokenType.*
import syspro.tm.lexer.{Lexer, Token}
import LiteralTokens.*
import IndentationProcessor.*

import java.util
import Symbols.*


case class Tokenizer() extends Lexer {
  var tokens: Tokens = Tokens()
  private var indents = IndentationProcessor()
  var idx = 0
  private var current_char: Char = '0'
  private var next_char: Char = '0'
  private var s: String = ""

  override def lex(str: String): java.util.List[Token] = {
    this.tokens = Tokens()
    indents = IndentationProcessor()
    idx = 0
    this.s = str

    while (idx < s.length) {
      current_char = s(idx)
      next_char = lookup(s, idx + 1).getOrElse('?')
      tokens.addChar(current_char)
      idx += 1
      if (isNewLine(tokens.sb)) {
        val nextString: String = extractNextString()
        if (nextString.isEmpty) {
          indents.updateLevel()
        }
        else if (!indents.hasIndentation(nextString)) {
          val numOfDedent = indents.dropLevel()
          tokens.add(idx, Dedent, numOfDedent)
        }
        else if (indents.hasOnlyWhitespaces(nextString)) {
          indents.updateLevel()
        }
        else {
          val numOfIndents = indents.countIndentation(nextString)
          val indentType = if (numOfIndents >= 0) Indent else Dedent
          tokens.add(idx, indentType, numOfIndents)
        }
        if (indents.isEndOfFile(s, nextString, idx)) {
          tokens.dedentsToFlush = indents.dropLevel()
        }
        tokens.dropStringBuilder()
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
      else if(isComment(tokens.sb)) {
        val extractedComment = extractComment()
        idx += extractedComment.length
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
        val extractedRuneInterior: String = extractRune()
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
        val extractedStringInterior: String = extractString()
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
    tokens.flush()
    return tokens.tokens
  }

  private def lookup(s: String, i: Int): Option[Char] = {
    if (i < s.length) {
      return Some(s(i))
    } else {
      return None
    }
  }

  private def extractString(): String = extract('"')

  private def extractRune(): String = extract("'"(0))

  private def extractNextString(): String = extract("\n"(0))

  private def extractComment(): String = extract("\n"(0))

  private def extract(StopChar: Char): String = {
    var i: Int = idx
    var extracted = ""
    while (i < s.length && s(i) != StopChar) {
      extracted += s(i)
      i += 1
    }
    return extracted
  }



}



