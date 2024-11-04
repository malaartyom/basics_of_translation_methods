package MyLexer

import MyLexer.Processors.IndentationProcessor.{getDedentType, getIndentType, hasIndentation, hasOnlyWhitespaces}
import syspro.tm.lexer.{Lexer, Token}
import MyLexer.Processors.{Extractor, IndentationProcessor, TokensProcessor, UnicodeProcessor}
import MyLexer.Tokens.Bad.isBad
import MyLexer.Tokens.Keywords.{isHardKeyword, isKeyword, isSoftKeyword}
import MyLexer.Tokens.LiteralTokens.{isBoolean, isInteger, isRuneInterior, isRuneStart, isStringInterior, isStringStart, isSuffix, notNull}
import MyLexer.Tokens.PrimitiveTokens.{isCarriageReturn, isComment, isIdentifier, isLongNewLine, isNewLine}
import MyLexer.Tokens.Symbols.{isLongSymbol, isShortSymbol}
import MyLexer.Tokens.TokenType.{Bad, BooleanLiteral, Dedent, HardKeyword, Identifier, Indent, IntegerLiteral, RuneLiteral, SoftKeyword, StringLiteral, Symbol}

import java.util


case class Tokenizer() extends Lexer with Extractor {

  private var unicodeProcessor: UnicodeProcessor = UnicodeProcessor()

  override def lex(str: String): java.util.List[Token] = {
    val unicodeProcessor = UnicodeProcessor(str)
    val tokens = TokensProcessor(str)
    val indents = IndentationProcessor()
    var current_char: String = ""
    var next_char: String = ""
    var idx = 0
    val s = str

    var longNewLine = false

    while (idx < unicodeProcessor.length) {
      current_char = unicodeProcessor.get(idx)
      next_char = unicodeProcessor.get(idx + 1)
      tokens.addChar(current_char)
      idx += 1
      if (isNewLine(tokens.sb) || isLongNewLine(tokens.sb, next_char)) {
        tokens.lastLineBreak = idx - 1

        if (isLongNewLine(tokens.sb, next_char)) {
          tokens.addChar(next_char)
          idx += 1
          tokens.addToTrivia(current_char)

        }

        val indentType = getIndentType(tokens.sb, idx - 1)
        val dedentType = getDedentType(tokens.sb, idx - 1)

        val nextString: String = extractNextString(s, idx)

        if (nextString.isEmpty) {
          indents.updateLevel()
        }
        else if (!hasIndentation(nextString)) {
          val numOfDedent = indents.dropLevel()
          val dedentList = indents.pushOrPop(idx, numOfDedent, dedentType)
          tokens.add(dedentList)
          //          tokens.add(idx, Dedent, numOfDedent, flushFlag = false)
        }
        else if (hasOnlyWhitespaces(nextString)) {
          indents.updateLevel()
        }
        else {
          val numOfIndents = indents.countIndentation(nextString)
          val indentsList = indents.pushOrPop(idx, numOfIndents, if (numOfIndents >= 0) indentType else dedentType)
          tokens.add(indentsList)
          //          tokens.add(idx, indentType, numOfIndents, flushFlag = false)
        }
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
        val extractedComment = extractComment(s, idx)
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
        val extractedRuneInterior: String = extractRune(s, idx)
        idx += UnicodeProcessor(extractedRuneInterior).length + 1
        tokens.dropStringBuilder()
        tokens.addString(extractedRuneInterior)
        if (!isRuneInterior(extractedRuneInterior)) {
          tokens.add(idx - 1, Bad)
        } else {
          tokens.add(idx - 1, RuneLiteral)
        }
        tokens.updateState()
      }
      else if (isStringStart(tokens.sb)) {
        val extractedStringInterior: String = extractString(s, idx)
        idx += UnicodeProcessor(extractedStringInterior).length
        tokens.dropStringBuilder()
        tokens.addString(extractedStringInterior)

        idx += 1
        if (!isStringInterior(extractedStringInterior)) {
          tokens.add(idx, Bad)
        } else {
          tokens.add(idx, StringLiteral)
        }
        tokens.updateState()
      }
      else if (isKeyword(tokens.sb) && !isKeyword(tokens.sb, next_char) && !isIdentifier(tokens.sb, next_char) ||
        idx == s.length && isKeyword(tokens.sb)) {
        if (isHardKeyword(tokens.sb)) {
          tokens.add(idx - 1, HardKeyword)
          tokens.updateState()
        } else if (isSoftKeyword(tokens.sb)) {
          tokens.add(idx - 1, SoftKeyword)
          tokens.updateState()
        }

      } else if (isIdentifier(tokens.sb) && !isIdentifier(tokens.sb, next_char) && !isHardKeyword(tokens.sb) && !isSoftKeyword(tokens.sb)) {
        tokens.add(idx - 1, Identifier)
        tokens.updateState()

      } else if (isBad(tokens.sb)) {
        val badToken = extractBad(s, idx)
        idx += badToken.length
        tokens.addString(badToken)
        tokens.add(idx - 1, Bad)
        tokens.updateState()
      }
    }
    val point = getPointToFlush(tokens, s)
    tokens.flush(indents.getCurrentIndentationLevel, indents.getStack, point, unicodeProcessor.length)
    //    tokens.flush(indents.getCurrentIndentationLevel, point)
    tokens.tokens
  }

  private def getPointToFlush(tokens: TokensProcessor, s: String): Int = {
    if (tokens.lastLineBreak == s.length - 1
      ||
      tokens.lastLineBreak == s.length - 2 &&
        isLongNewLine(s(tokens.lastLineBreak).toString, s(tokens.lastLineBreak + 1).toString)
    ) {
      tokens.lastLineBreak
    } else if (isLongNewLine(s(tokens.lastLineBreak).toString, s(tokens.lastLineBreak + 1).toString)
      && hasOnlyWhitespaces(s.slice(tokens.lastLineBreak + 2, s.length)) ||
      hasOnlyWhitespaces(s.slice(tokens.lastLineBreak + 1, s.length))) {
      tokens.lastLineBreak
    } else {
      tokens.trueEnd + 1
    }
  }


  private def extractBad(s: String, idx: Int): String = extract(s = s, idx = idx, stop = "", function = (x, y) => isBad(x))

  private def extractString(s: String, idx: Int): String = extract(s = s, idx = idx, stop = "\"")

  private def extractRune(s: String, idx: Int): String = extract(s = s, idx = idx, stop = "'")

  private def extractNextString(s: String, idx: Int): String = extract(s = s, idx = idx, stop = "\n\r")

  private def extractComment(s: String, idx: Int): String = extract(s = s, idx = idx, stop = "\n\r")

  override def extract(s: String, stop: String, idx: Int, function: (String, String) => Boolean = (x, y) => !y.contains(x)): String = super.extract(s, stop, idx, function)
}







