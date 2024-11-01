import IndentationProcessor.{hasIndentation, hasOnlyWhitespaces}
import Keywords.{isHardKeyword, isKeyword, isSoftKeyword}
import PrimitiveTokens.{isComment, isEndOfFile, isIdentifier, isNewLine, isTrivia}
import TokenType.*
import syspro.tm.lexer.{Lexer, Token}
import LiteralTokens.{notNull, *}
import UnicodeProcessor.*

import java.util
import Symbols.*


case class Tokenizer() extends Lexer with Extractor{
  var tokens: Tokens = Tokens()
  private var indents = IndentationProcessor()
  var idx = 0
  private var current_char: String = ""
  private var next_char: String = ""
  private var s: String = ""

  private var unicodeProcessor: UnicodeProcessor = UnicodeProcessor()

  override def lex(str: String): java.util.List[Token] = {
    unicodeProcessor = UnicodeProcessor(str)
    this.tokens = Tokens(str)
    indents = IndentationProcessor()
    idx = 0
    this.s = str

    while (idx < unicodeProcessor.length) {
      current_char = unicodeProcessor.get(idx)
      next_char = unicodeProcessor.get(idx + 1)
      tokens.addChar(current_char)
      idx += 1
      if (isNewLine(tokens.sb)) {
        val nextString: String = extractNextString()
        if (nextString.isEmpty) {
          indents.updateLevel()
        }
        else if (!hasIndentation(nextString)) {
          val numOfDedent = indents.dropLevel()
          tokens.add(idx, Dedent, numOfDedent, flushFlag = false)
        }
        else if (hasOnlyWhitespaces(nextString)) {
          indents.updateLevel()
        }
        else {
          val numOfIndents = indents.countIndentation(nextString)
          val indentType = if (numOfIndents >= 0) Indent else Dedent
          tokens.add(idx, indentType, numOfIndents, flushFlag = false)
        }
        tokens.lastLineBreak = idx - 1
        tokens.dropStringBuilder()
      }
      else if (isLongSymbol(tokens.sb, next_char)) {
        tokens.addChar(next_char)
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
        idx += extractedComment.length
        tokens.dropStringBuilder()

      }
      else if (isBoolean(tokens.sb)) {
        tokens.add(idx - 1, BooleanLiteral)
        tokens.updateState()
      }
      else if (isInteger(tokens.sb) && !isInteger(tokens.sb, next_char)) {
        val suffix: String = s.slice(idx, idx + 3) // TODO Use Unicode
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
        tokens.dropStringBuilder()
        tokens.addString(extractedStringInterior)
        if (!isStringInterior(extractedStringInterior)) {
          tokens.add(idx, Bad)
        } else {
          tokens.add(idx, StringLiteral)
        }
        tokens.updateState()
      }
      else if (isKeyword(tokens.sb) && !isKeyword(tokens.sb,  next_char) && !isIdentifier(tokens.sb, next_char) || idx == s.length) {
        if (isHardKeyword(tokens.sb)) {
          tokens.add(idx - 1, HardKeyword)
          tokens.updateState()
        } else if (isSoftKeyword(tokens.sb)) {
          tokens.add(idx - 1, SoftKeyword)
          tokens.updateState()
        }

      } else if (isIdentifier(tokens.sb)  && !isHardKeyword(tokens.sb) && !isSoftKeyword(tokens.sb)) {
        if (notNull(next_char) && !isIdentifier(tokens.sb + next_char)) {
          tokens.add(idx - 1, Identifier)
          tokens.updateState()
        }
      }
    }
    tokens.flush(indents.getCurrentIndentationLevel, if (tokens.lastLineBreak == s.length - 1) tokens.lastLineBreak else tokens.trueEnd + 1)
    return tokens.tokens
  }

  private def extractString(): String = extract(stop=""""""")

  private def extractRune(): String = extract(stop="'")

  private def extractNextString(): String = extract(stop="\n")

  private def extractComment(): String = extract(stop="\n")

  override def extract(s: String = this.s, stop: String, idx: Int = this.idx: Int, function: (String, String) => Boolean = (x, y) => x != y): String = super.extract(s, stop, idx, function)
}







