package MyParser

import syspro.tm.lexer.{IdentifierToken, Keyword, Token}
import syspro.tm.lexer.Keyword.*
import MyLexer.Tokens.LiteralTokens.isNull

object ParserUtils {
  def isTypeDefStart(token: Token): Boolean =
    token.isInstanceOf[IdentifierToken] && isTypeDefinitionKeyword(token.asInstanceOf[IdentifierToken].contextualKeyword)

  def isIdentifier(token: Token): Boolean = 
    token.isInstanceOf[IdentifierToken] && token.asInstanceOf[IdentifierToken].contextualKeyword == null 


  private def isTypeDefinitionKeyword(keyword: Keyword): Boolean =
    keyword match
      case CLASS | OBJECT | INTERFACE => true
      case _ => false

  def isTypeParam(token: Token): Boolean = return false

  def isTypeBound(token: Token): Boolean = return false



}
