package MyLexer.Processors

import MyLexer.Tokens.TokenType.{Dedent, Indent}
import MyLexer.Tokens.{TokenType, Trivia}
import syspro.tm.lexer.{IndentationToken, Token}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


case class IndentationProcessor() extends Extractor {

  var lastLineBreak = 0
  private var currentIndentationLevel = 0
  private var currentIndentationLength = -1

  def getCurrentIndentationLevel: Int = currentIndentationLevel


  def pushOrPop(idx: Int, num: Int, indentType: TokenType): ListBuffer[Token] = {
    val returnList = ListBuffer[Token]()
    for (_ <- 1 to num.abs) {
      indentType match
        case Indent(len, start, end) => returnList.append(new IndentationToken(start, end, 0, 0, 1))
        case Dedent(len, start, end) => returnList.append(new IndentationToken(start, end, 0, 0, -1))
    }
    returnList
  }
  

  def dropLevel(): Int = {
    val tmp = currentIndentationLevel
    currentIndentationLevel = 0
    -tmp
  }

  def updateLevel(): Unit = {
    currentIndentationLevel = currentIndentationLevel
    currentIndentationLength = currentIndentationLength
  }

  def countIndentation(s: String): Int = {
    val indent: String = IndentationProcessor.extract(s)
    if (indent.length % 2 != 0) {
      updateLevel()
      return 0
    }
    else if (currentIndentationLevel == 0 && indent.nonEmpty) {
      currentIndentationLength = indent.length
      currentIndentationLevel = 1
      return 1
    }
    else if (indent.length % currentIndentationLength != 0) {
      updateLevel()
      return 0
    }
    else if (indent.isEmpty) {
      val numOdDedent = dropLevel()
      return numOdDedent
    }
    else if (indent.length % currentIndentationLength == 0) {
      val N = indent.length / currentIndentationLength
      val prevIndentationLevel = currentIndentationLevel
      currentIndentationLevel = N
      return currentIndentationLevel - prevIndentationLevel
    }
    0
  }

}

object IndentationProcessor extends Extractor:

  private val WHITESPACE: String = " "
  private val TABULATION: String = """\t"""

  override def extract(s: String, stop: String = " ", idx: Int = 0, function: (String, String) => Boolean): String =
    super.extract(s, " ", 0, (x, y) => x == y)

  def hasOnlyWhitespaces(s: String): Boolean = Trivia.WHITESPACE.matches(s)

  def hasIndentation(s: String): Boolean = s.startsWith(WHITESPACE) || s.startsWith(TABULATION)
  

  def isSyntheticToken(token: Token): Boolean = token.isInstanceOf[IndentationToken]

  def isSyntheticToken(token: TokenType): Boolean = token == Indent || token == Dedent

  def getIndentType(s: String, idx: Int): TokenType = {
    s match
      case "\r\n" => Indent(2, idx - 1, idx)
      case "\n" => Indent(1, idx, idx)
  }

  def getDedentType(s: String, idx: Int): TokenType = {
    s match
      case "\r\n" => Dedent(2, idx - 1, idx)
      case "\n" => Dedent(1, idx, idx)
  }


end IndentationProcessor
