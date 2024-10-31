import Trivia.WHITESPACE
import StringLiteral.STRING
import syspro.tm.lexer.Token

import scala.util.matching.Regex

object Main {
  def main(args: Array[String]): Unit = {
    var lexer = Tokenizer()
//    syspro.tm.Tasks.Lexer.registerSolution(lexer)
    println(lexer.lex(
      """""".stripMargin))
    var s = """class Indent5
              |  def memberIsAt2(): Boolean
              |    return true
              |      # Comment introduced identation level in the method body (EOF rule is not applicable here)""".stripMargin

    println(s.length)
    println(s.slice(840, 890))
    println(lexer.lex(s))
    printTokens(lexer.lex(s))
    //    println(STRING.matches(""""Hifjn wuhuhwuf jjuh wuhubbfuhwuhfuwbfubfqn26378e9uf9jedndsa$T bro""""))
    val INTEGER: Regex = "[0-9]+(i32|i64|u32|u64)?".r
  }

  def printTokens(l: java.util.List[Token]): Unit = {
    var i: Int = 0
    (0 until l.size())
      .foreach(i =>

        print(l.get(i).start.toString + " " +  l.get(i).end.toString + " " + l.get(i).leadingTriviaLength.toString + " " + l.get(i).trailingTriviaLength + "\n")
      )
  }
}

