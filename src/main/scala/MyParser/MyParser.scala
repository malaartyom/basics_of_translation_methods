package MyParser

import syspro.tm.parser.Parser
import syspro.tm.parser.ParseResult
import syspro.tm.lexer.Keyword.*
import MyLexer.Tokenizer
import syspro.tm.lexer.{Keyword, Token}
import syspro.tm.parser.SyntaxKind.*

import scala.jdk.CollectionConverters.*
import java.util
import scala.collection.mutable

import ParserUtils.*

case class MyParser() extends Parser {

  private var state = State()
  
  private def matchTypeDef(tokens: Vector[Token]): MySyntaxNode = {
    val node = MySyntaxNode(TYPE_DEFINITION)

    if (isTypeDefStart(tokens(state.idx))) {
      node.add(CLASS, tokens(state.idx))
      state.idx += 1
    } else {
      ???
    }
    if (isIdentifier(tokens(state.idx))) {
      node.add(IDENTIFIER, tokens(state.idx))
      state.idx += 1
    } else {
      ???
    }
    if (isTypeParam(tokens(state.idx))) {
      node.add(matchTypeParam(tokens))
      state.idx += 1
    }
    if (isTypeBound(tokens(state.idx))) {
      node.add(matchTypeBound(tokens))
      state.idx += 1
    }
    matchMemberBlock(tokens)
    node
  }
  
  private def matchMemberBlock(tokens: Vector[Token]): MySyntaxNode = ???
  
  private def matchTypeBound(token: Vector[Token]): MySyntaxNode = ???
  
  private def matchTypeParam(tokens: Vector[Token]): MySyntaxNode = ???

  

  def update(tokens: Vector[Token]): Vector[Token] = {
    tokens
  }

  override def parse(s: String): ParseResult = {
    val lexer = Tokenizer()
    val tokens: Vector[Token] = lexer.lex(s).asScala.toVector
    val parseResult = MyParseResult(SOURCE_TEXT)


    while (state.idx < tokens.length) {
      parseResult.addToRoot(matchTypeDef(tokens))
      state.idx += 1
    }
    parseResult
  }
}
