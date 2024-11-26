package ParserImplementation

import syspro.tm.lexer.{BooleanLiteralToken, IdentifierToken, IndentationToken, IntegerLiteralToken, Keyword, KeywordToken, RuneLiteralToken, StringLiteralToken, Symbol, SymbolToken, Token}
import syspro.tm.lexer.Keyword.*
import syspro.tm.lexer.Symbol.*
import LexerImplementation.Tokens.LiteralTokens.isNull

import scala.collection.mutable

object Checkers {

  def isTypeDefStart(token: Token): Boolean =
    token.isInstanceOf[IdentifierToken] && isTypeDefinitionKeyword(token.asInstanceOf[IdentifierToken].contextualKeyword)

  def isIdentifier(token: Token): Boolean =
    token.isInstanceOf[IdentifierToken] && token.asInstanceOf[IdentifierToken].contextualKeyword == null


  def isTypeDefinitionKeyword(keyword: Keyword): Boolean =
    keyword match
      case CLASS | OBJECT | INTERFACE => true
      case _ => false


  def isTypeBound(token: Token): Boolean =
    token.isInstanceOf[SymbolToken] && token.asInstanceOf[SymbolToken].symbol == Symbol.BOUND

  def isTypeParameterDef(token: Token): Boolean = isIdentifier(token)

  def isNameExpression(token: Token): Boolean = isIdentifier(token) || isSymbol(token, Symbol.QUESTION)

  def isExpression(token: Token): Boolean = {
    token match
      case identifierToken: IdentifierToken if identifierToken.contextualKeyword != null =>
        identifierToken.contextualKeyword match
          case NULL => true
          case _ => false
      case keywordToken: KeywordToken =>
        keywordToken.keyword match
          case THIS | SUPER => true
          case _ => false
      case symbol: SymbolToken =>
        symbol.symbol match
          case EXCLAMATION | PLUS | MINUS | TILDE | OPEN_PAREN | QUESTION => true
          case _ => false
      case identifier: IdentifierToken => true
      case rune: RuneLiteralToken => true
      case boolean: BooleanLiteralToken => true
      case string: StringLiteralToken => true
      case int: IntegerLiteralToken => true
      case _ => false
  }


  def isIndent(token: Token): Boolean = token.isInstanceOf[IndentationToken] && token.asInstanceOf[IndentationToken].isIndent

  def isDedent(token: Token): Boolean = token.isInstanceOf[IndentationToken] && token.asInstanceOf[IndentationToken].isDedent


  def isVariableDefStart(token: Token): Boolean = {
    token match
      case token1: KeywordToken =>
        token1.keyword match
          case VAR | VAL => true
          case _ => false
      case _ =>
        false

  }

  def isFunctionDefStart(token: Token): Boolean = {
    token match
      case token1: KeywordToken =>
        token1.keyword match
          case ABSTRACT | VIRTUAL | OVERRIDE | NATIVE | DEF => true
          case _ => false
      case _ => false

  }

  def isSymbol(token: Token, symbol: Symbol = null): Boolean =
    if (symbol == null) {
      token.isInstanceOf[SymbolToken]
    } else {
      token.isInstanceOf[SymbolToken] && token.asInstanceOf[SymbolToken].symbol == symbol
    }

  def isKeyword(token: Token, keyword: Keyword = null): Boolean = {
    if (keyword == null) {
      token.isInstanceOf[KeywordToken]
    } else {
      token.isInstanceOf[KeywordToken] && token.asInstanceOf[KeywordToken].keyword == keyword
    }
  }


  def isDefinition(token: Token): Boolean = {
    token match
      case typeDef: IdentifierToken if isTypeDefStart(typeDef) => true
      case funcDef: KeywordToken if isFunctionDefStart(funcDef) || isKeyword(funcDef, DEF) => true
      case varDef: KeywordToken if isVariableDefStart(varDef) => true
      case parameterDef: IdentifierToken if isIdentifier(parameterDef) => true
      case _ => false
  }

  def isPrimary(token: Token): Boolean = {
    token match
      case nameExpression if isNameExpression(nameExpression) => true
      case identifierToken: IdentifierToken if identifierToken.contextualKeyword != null =>
        identifierToken.contextualKeyword match
          case NULL => true
          case _ => false
      case keywordToken: KeywordToken =>
        keywordToken.keyword match
          case THIS | SUPER => true
          case _ => false
      case symbol: SymbolToken =>
        symbol.symbol match
          case OPEN_PAREN => true
          case _ => false
      case rune: RuneLiteralToken => true
      case boolean: BooleanLiteralToken => true
      case string: StringLiteralToken => true
      case int: IntegerLiteralToken => true
      case _ => false
  }

  def isContinueOfPrimary(token: Token): Boolean = isSymbol(token, Symbol.OPEN_BRACKET) || isSymbol(token, Symbol.OPEN_PAREN) || isSymbol(token, Symbol.DOT)

  def isUnary(token: Token): Boolean = isSymbol(token, PLUS) || isSymbol(token, MINUS) || isSymbol(token, TILDE) || isSymbol(token, EXCLAMATION)

  def isOperation(token: Token): Boolean = {
    token match
      case symbolToken: SymbolToken => symbolToken.symbol match
        case AMPERSAND_AMPERSAND | BAR_BAR | EQUALS_EQUALS |
             EXCLAMATION_EQUALS | LESS_THAN | LESS_THAN_EQUALS |
             GREATER_THAN | GREATER_THAN_EQUALS | AMPERSAND | BAR |
             CARET | LESS_THAN_LESS_THAN | GREATER_THAN_GREATER_THAN |
             PLUS | MINUS | ASTERISK | SLASH | PERCENT => true

  }

  def isExpressionContinue(token: Token): Boolean =
    token match
      case symbolToken: SymbolToken if isOperation(symbolToken) => true
      case keywordToken: KeywordToken => keywordToken.keyword match
        case IS => true
        case _ => false
      case x if isPrimary(x) => true

  def isStatement(token: Token): Boolean =
    val tmp = token match {
      case keyword: KeywordToken => keyword.keyword match
        case RETURN | BREAK | CONTINUE | IF | WHILE | FOR => true
        case _ => false
      case _ => false
    }
    isVariableDefStart(token)|| isPrimary(token) || isExpression(token) || tmp
}
