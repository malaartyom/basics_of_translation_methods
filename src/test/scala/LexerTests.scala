import LexerImplementation.Tokenizer

class LexerTests extends munit.FunSuite {
  test("Escape Seq") {
    val t = Tokenizer()
    val s = "return \"\\U+1D6A8\\U+00AD\\U+10341\""
    val tokens = t.lex(s)

    println(tokens)
  }

}
