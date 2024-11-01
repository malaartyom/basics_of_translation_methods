import Keywords.{isHardKeyword, isKeyword, isSoftKeyword}
import PrimitiveTokens.{isComment, isEndOfFile, isIdentifier, isNewLine, isTrivia}
import TokenType.*
import syspro.tm.lexer.{Lexer, Token}
import LiteralTokens.*
import IndentationProcessor.*
import UnicodeProcessor._

import java.util
import Symbols.*


case class Tokenizer() extends Lexer {
  var tokens: Tokens = Tokens()
  private var indents = IndentationProcessor()
  var idx = 0
  private var current_char: String = ""
  private var next_char: String = ""
  private var s: String = ""

  override def lex(str: String): java.util.List[Token] = {
    val unicodeProcessor = UnicodeProcessor(str)

    next_char = unicodeProcessor.get()
    this.tokens = Tokens(str)
    indents = IndentationProcessor()
    idx = 0
    this.s = str

    while (unicodeProcessor.hasNext()) {
      current_char = next_char
      next_char = unicodeProcessor.get()
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
        if (isEndOfFile(s, nextString, idx)) {
          tokens.dedentsToFlush = indents.dropLevel()
          if (isTrivia(nextString)) {
            tokens.lastLineBreak = idx - 1
          } else {
            tokens.lastLineBreak = -1
          }
        }
        tokens.dropStringBuilder()
      }
      else if (isLongSymbol(tokens.sb + next_char)) {
        tokens.addChar(next_char)
        current_char = next_char
        next_char = unicodeProcessor.get()
        idx += 1
        tokens.add(idx - 1, Symbol)
        tokens.updateState()
      }
      else if (isShortSymbol(tokens.sb)) {
        tokens.add(idx - 1, Symbol)
        tokens.updateState()
      }
      else if (isComment(tokens.sb)) {
        val extractedComment = extractComment()
        tokens.addToTrivia(extractedComment)
        current_char = unicodeProcessor.get(extractedComment.length - 1) // TODO Fix
        next_char = unicodeProcessor.get()
        idx += extractedComment.length
        tokens.dropStringBuilder()

      }
      else if (isBoolean(tokens.sb)) {
        tokens.add(idx - 1, BooleanLiteral)
        tokens.updateState()
      }
      else if (isInteger(tokens.sb) && !isInteger(tokens.sb + next_char)) {
        val suffix: String = s.slice(idx, idx + 3) // TODO Use Iterator
        if (isSuffix(suffix)) {
          tokens.addString(suffix)
          current_char = unicodeProcessor.get(2) // TODO Fix
          next_char = unicodeProcessor.get()
          idx += 3
        }
        tokens.add(idx - 1, IntegerLiteral)
        tokens.updateState()
      }
      else if (isRuneStart(tokens.sb)) {
        val extractedRuneInterior: String = extractRune()
        current_char = unicodeProcessor.get(extractedRuneInterior.length) // TODO Fix
        next_char = unicodeProcessor.get()
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
        current_char = unicodeProcessor.get(extractedStringInterior.length) // TODO Fix
        next_char = unicodeProcessor.get()
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
    tokens.flush(idx)
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
    while (i < s.length && s(i) != StopChar) { // TODO use iterator
      extracted += s(i)
      i += 1
    }
    return extracted
  }


}



