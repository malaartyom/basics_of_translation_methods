import Trivia.WHITESPACE

object Main {
  def main(args: Array[String]): Unit = {
    var lexer = Tokenizer()
    println(WHITESPACE.matches(" "))
    println(lexer.lex("var   \n \n val return"))

  }
}

