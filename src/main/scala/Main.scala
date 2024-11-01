import Trivia.TRIVIA
import syspro.tm.lexer.Token

import scala.util.matching.Regex

object Main {
  def main(args: Array[String]): Unit = {
    val lexer = Tokenizer()
    val x = UnicodeProcessor("\uD835\uDEA8\u00AD\uD800\uDF41").length
    println(x)
    syspro.tm.Tasks.Lexer.registerSolution(lexer)
    println(TRIVIA.matches("""
                             |      # Comment introduced identation level in the method body (EOF rule is not applicable here)""".stripMargin))
    val s =
      """class 𝚨­𐍁
        |    def nameImplicit(): String
        |        return "𝚨­𐍁"
        |    def nameExͯplicit(): String
        |        return "\U+1D6A8\U+00AD\U+10341"
        |    def letterImplicit(): Rune
        |        return '𝚨'
        |    def letterExͯplicit(): Rune
        |        return '\U+1D6A8'
        |    def number﻿Value(): Int64
        |        return 90
        |    def numberImplicit(): Rune
        |        return '𐍁'
        |    def numberExͯplicit(): Rune
        |        return '\U+10341'""".stripMargin

    println(s.length)
    println(s.slice(840, 890))
    println(lexer.lex(s))
    printTokens(lexer.lex(s))
    //    println(STRING.matches(""""Hifjn wuhuhwuf jjuh wuhubbfuhwuhfuwbfubfqn26378e9uf9jedndsa$T bro""""))
    val INTEGER: Regex = "[0-9]+(i32|i64|u32|u64)?".r
  }

  private def printTokens(l: java.util.List[Token]): Unit = {
    var i: Int = 0
    (0 until l.size())
      .foreach(i =>

        print(l.get(i).start.toString + " " +  l.get(i).end.toString + " " + l.get(i).leadingTriviaLength.toString + " " + l.get(i).trailingTriviaLength + "\n")
      )
  }
}

