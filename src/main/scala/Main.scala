import MyLexer.Tokenizer
import syspro.tm.lexer.TestLineTerminators.{CarriageReturnLineFeed, LineFeed, Mixed, Native}
import syspro.tm.lexer.{TestMode, Token}

import scala.util.matching.Regex
object Main {
  def main(args: Array[String]): Unit = {
    val lexer = Tokenizer()
    val test = TestMode()
//    syspro.tm.Tasks.Lexer.registerSolution(lexer, test.forceLineTerminators(Mixed))
    val s = """
              |#xyz
              |
              |
              |var x = 1; #jdijw
              |  2hd  №№@@ 123
              |    №""".stripMargin

    val y = """'\v' '\a' '\\' '5' 'd'
'd'"""
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

