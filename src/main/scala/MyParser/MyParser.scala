package MyParser

import syspro.tm.parser.Parser
import syspro.tm.parser.ParseResult
import MyLexer.Tokenizer
import syspro.tm.lexer.{Keyword, Token}
import syspro.tm.parser.SyntaxKind

import java.util

case class MyParser() extends Parser {
  override def parse(s: String): ParseResult = {
    val lexer = Tokenizer()
    val tokens: util.List[Token] = lexer.lex(s)
    CoolParseResult(SyntaxKind.BAD, tokens.get(0))
  }
}
