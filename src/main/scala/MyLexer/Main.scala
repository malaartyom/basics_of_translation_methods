package MyLexer

import syspro.tm.lexer.Token
import MyLexer.Tokenizer

import scala.util.matching.Regex
import syspro.tm.lexer.TestMode
import syspro.tm.lexer.TestLineTerminators.{Native, LineFeed, CarriageReturnLineFeed, Mixed}
object Main {
  def main(args: Array[String]): Unit = {
    val lexer = Tokenizer()
    val test = TestMode()
    syspro.tm.Tasks.Lexer.registerSolution(lexer, test.forceLineTerminators(CarriageReturnLineFeed))
    val s = """class Indent6
              |  def memberIsAt2(): Boolean
              |    return true
              |    # The spaces in the following line are ignored for identation purposes,
              |    # as per EOF rule
              |        """.stripMargin

    val y = """' "\U+12DA \n \r \" \U+32AD" """

    val z =
      """
        |
        |@@@
        |
        |var z = 1
        |123a12
        |
        |
        |  x = 0
        |    @@""".stripMargin
    println(lexer.lex(y))
    printTokens(lexer.lex(y))
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

