package MyLexer

import syspro.tm.lexer.Token
import MyLexer.Tokenizer

import scala.util.matching.Regex
import syspro.tm.lexer.TestMode
import syspro.tm.lexer.TestLineTerminators.{Native, LineFeed, CarriageReturnLineFeed, Mixed}

object Main {
  def main(args: Array[String]): Unit = {
    val lexer = Tokenizer()
    var test = TestMode()
    test = test.repeated(false)
    test = test.parallel(true)
    test = test.shuffled(true)
    test = test.forceLineTerminators(Mixed)
    syspro.tm.Tasks.Lexer.registerSolution(lexer, test)
//    val s = """class Indent6
//              |  def memberIsAt2(): Boolean
//              |    return true
//              |    # The spaces in the following line are ignored for identation purposes,
//              |    # as per EOF rule
//              |        """.stripMargin
//
//    val y = ""

//    val z =
//      """
//        |
//        |@@@
//        |
//        |var z = 1
//        |123a12
//        |
//        |
//        |  x = 0
//        |    @@""".stripMargin
//    println(lexer.lex(x))
//    printTokens(lexer.lex(s))
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

