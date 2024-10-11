import PrimitiveTokens.isTrivia
import syspro.tm.lexer.{Lexer, Token}

import java.util


class Tokenizer extends Lexer {

  override def lex(s: String): java.util.List[Token] = {

    new util.ArrayList[Token]()
  }


}



