import Trivia.WHITESPACE
import StringLiteral.STRING
import syspro.tm.lexer.Token

import scala.util.matching.Regex

object Main {
  def main(args: Array[String]): Unit = {
    var lexer = Tokenizer()
//    syspro.tm.Tasks.Lexer.registerSolution(lexer)
    println(lexer.lex("""var x = 'b'  '\n'  var govno = "hui"   """))
    println(lexer.lex("""for i in range(0, 10): \n    print(X)"""))
    println(lexer.lex(
      """var xyz=123u32+124u32   #  bro
        |class Object
        |
        |""".stripMargin))
    var s = """class Indent1
              |   def notMultipleOf2(): Boolean
              |      return true""".stripMargin

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

