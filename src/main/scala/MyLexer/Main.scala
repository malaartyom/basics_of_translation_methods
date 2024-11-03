package MyLexer

import syspro.tm.lexer.Token

import scala.util.matching.Regex
import syspro.tm.lexer.TestMode
import syspro.tm.lexer.TestLineTerminators.{Native, LineFeed, CarriageReturnLineFeed, Mixed}
object Main {
  def main(args: Array[String]): Unit = {
    val lexer = Tokenizer()
    val test = TestMode()
    syspro.tm.Tasks.Lexer.registerSolution(lexer, test.forceLineTerminators(Mixed))
    val s = """class Indent5
        |  def memberIsAt2(): Boolean
        |    return true
        |      # Comment introduced identation level in the method body (EOF rule is not applicable here)""".stripMargin
    println(lexer.lex(s))
    printTokens(lexer.lex(s))
  }

  private def printTokens(l: java.util.List[Token]): Unit = {
    var i: Int = 0
    (0 until l.size())
      .foreach(i =>

        print(i.toString + " " +
          l.get(i).toString + " " +  l.get(i).start.toString + " " +  l.get(i).end.toString + " " + l.get(i).leadingTriviaLength.toString + " " + l.get(i).trailingTriviaLength + "\n")
      )
  }
}

// parallel