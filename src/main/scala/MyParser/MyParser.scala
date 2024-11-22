package MyParser

import syspro.tm.parser.Parser
import syspro.tm.parser.ParseResult
import syspro.tm.lexer.Keyword.*
import syspro.tm.lexer.Symbol.*
import MyLexer.Tokenizer
import syspro.tm.lexer.{IdentifierToken, IndentationToken, Keyword, KeywordToken, Symbol, SymbolToken, Token}
import syspro.tm.parser.SyntaxKind.*

import scala.jdk.CollectionConverters.*
import ParserUtils.*

import scala.Predef.???

case class MyParser() extends Parser {

  var state = State()

  def matchTypeBound(tokens: Vector[Token], state: State): MySyntaxNode = {
    val node = MySyntaxNode(TYPE_BOUND)
    node.add(Symbol.BOUND, tokens(state.idx)) // TODO: Check if not BOUND ??? Same in all other match* functions
    state.idx += 1

    if (isNameExpression(tokens(state.idx))) {

      val sepList = MySyntaxNode(SEPARATED_LIST)

      sepList.add(matchNameExpression(tokens))

      while (state.idx < tokens.length && isSymbol(tokens(state.idx), Symbol.AMPERSAND)) {
        sepList.add(Symbol.AMPERSAND, tokens(state.idx))
        state.idx += 1

        if (isNameExpression(tokens(state.idx))) {
          sepList.add(matchNameExpression(tokens))
        }
      }
      node.add(sepList)
    }
    node
  }

  def matchDefinition(tokens: Vector[Token]): MySyntaxNode = {
    tokens(state.idx) match
      case typeDef: IdentifierToken if isTypeDefStart(typeDef) => matchTypeDef(tokens)
      case funcDef: KeywordToken if isFunctionDefStart(funcDef) => matchFuncDef(tokens)
      case varDef: KeywordToken if isVariableDefStart(varDef) => matchVariableDef(tokens)
      case typeParameterDef: IdentifierToken if isIdentifier(typeParameterDef) => matchTypeParamDef(tokens)
      case parameterDef: IdentifierToken if isIdentifier(parameterDef) && state.idx + 1 < tokens.length
        && isSymbol(tokens(state.idx + 1), Symbol.COLON) => matchParamDef(tokens)
  }

  def matchTypeDef(tokens: Vector[Token]): MySyntaxNode = {
    val node = MySyntaxNode(TYPE_DEFINITION)

    if (isTypeDefStart(tokens(state.idx))) {
      val terminal = matchTypeDefStart(tokens)
      node.add(terminal, tokens(state.idx))
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

    if (isSymbol(tokens(state.idx), Symbol.LESS_THAN)) {
      node.add(Symbol.LESS_THAN, tokens(state.idx))
      state.idx += 1

      if (isTypeParameterDef(tokens(state.idx))) {
        val sepList = MySyntaxNode(SEPARATED_LIST)
        sepList.add(matchTypeParamDef(tokens))

        while (isSymbol(tokens(state.idx), Symbol.COMMA)) {
          sepList.add(Symbol.COMMA, tokens(state.idx))
          state.idx += 1
          if (isTypeParameterDef(tokens(state.idx))) {
            sepList.add(matchTypeParamDef(tokens))
          } else {
            // TODO: что-то хуёвое надо пропустить запятую и дальше парсить
          }
        }
        node.add(sepList)
      }

      if (isSymbol(tokens(state.idx), Symbol.GREATER_THAN)) {
        node.add(Symbol.GREATER_THAN, tokens(state.idx))
        state.idx += 1
      }
    }
    if (isTypeBound(tokens(state.idx))) {
      node.add(matchTypeBound(tokens, state))
      state.drop()
    }
    if (state.idx < tokens.length && isIndent(tokens(state.idx))) { // TODO: Do same thing with other conditions
      node.add(INDENT, tokens(state.idx))
      state.idx += 1
      if (isDefinition(tokens(state.idx))) {
        node.add(matchDefinition(tokens))
      }
      if (isDedent(tokens(state.idx))) {
        node.add(DEDENT, tokens(state.idx))
        state.idx += 1
      } else {
        // Нет Dedenta но есть Indent очеьн плохо
      }
    } else {
      // Empty member block
    }
    node
  }

  def matchTypeDefStart(tokens: Vector[Token]): Keyword = tokens(state.idx).asInstanceOf[IdentifierToken].contextualKeyword

  def matchFuncDef(tokens: Vector[Token]): MySyntaxNode = {
    val node = MySyntaxNode(FUNCTION_DEFINITION)
    while (isFunctionDefStart(tokens(state.idx))) {
      node.add(matchFuncDefStart(tokens))
    }
    if (isKeyword(tokens(state.idx), DEF)) {
      node.add(DEF, tokens(state.idx)) // TODO: DEF instead of keyword.DEF
      state.idx += 1
    }
    if (isIdentifier(tokens(state.idx))) {
      node.add(IDENTIFIER, tokens(state.idx))
      state.idx += 1
    } else if (isKeyword(tokens(state.idx), THIS)) {
      node.add(THIS, tokens(state.idx))
      state.idx += 1
    } else {
      // Bad Situation
    }

    if (isSymbol(tokens(state.idx), OPEN_PAREN)) {
      node.add(OPEN_PAREN, tokens(state.idx))
      state.idx += 1
    } else {
      // Bad Situation
    }

    if (isIdentifier(tokens(state.idx))) {
      val sepList = MySyntaxNode(SEPARATED_LIST)
      sepList.add(matchParamDef(tokens))
      while (isSymbol(tokens(state.idx), COMMA)) {
        sepList.add(COMMA, tokens(state.idx))
        state.idx += 1
        if (isIdentifier(tokens(state.idx))) {
          sepList.add(matchParamDef(tokens))
        }
        else {
          // Есть запятая но нет дальше ничего
        }
      }
      node.add(sepList)
    }

    if (isSymbol(tokens(state.idx), CLOSE_PAREN)) {
      node.add(CLOSE_PAREN, tokens(state.idx))
      state.idx += 1
    } else {
      // Не хватает закрывающейся скобочки
    }

    if (isSymbol(tokens(state.idx), COLON)) {
      node.add(COLON, tokens(state.idx))
      state.idx += 1
      if (isNameExpression(tokens(state.idx))) {
        node.add(matchNameExpression(tokens))
      } else {
        // Не хватает типа. Короче какая-то хуета
      }
    }

    if (isIndent(tokens(state.idx))) {
      node.add(INDENT, tokens(state.idx))
      state.idx += 1
      val list = MySyntaxNode(LIST)
      while (isStatement(tokens(state.idx))) {
        list.add(matchStatement(tokens))
      }
      node.add(list)
      if (isDedent(tokens(state.idx))) {
        node.add(DEDENT, tokens(state.idx))
      } else {
        // Нет Dedenta но есть Indent очень плохо
      }
    }
    node
  }

  def matchFuncDefStart(tokens: Vector[Token]): MySyntaxNode = {
    var node = MySyntaxNode(BAD)
    tokens(state.idx) match
      case keyword: KeywordToken =>
        keyword.keyword match
          case ABSTRACT | VIRTUAL | OVERRIDE | NATIVE => node = MySyntaxNode(keyword.keyword, tokens(state.idx));
      case _ =>
    state.idx += 1
    node
  }

  def matchVariableDef(tokens: Vector[Token]): MySyntaxNode = {
    val node = MySyntaxNode(VARIABLE_DEFINITION)
    if (isVariableDefStart(tokens(state.idx))) {
      node.add(matchVariableDefStart(tokens, state))
    } else {
      ???
    }
    if (isIdentifier(tokens(state.idx))) {
      node.add(IDENTIFIER, tokens(state.idx))
      state.idx += 1
    } else {
      ???
    }
    if (state.idx < tokens.length && isSymbol(tokens(state.idx), Symbol.COLON)) {
      node.add(Symbol.COLON, tokens(state.idx))
      state.idx += 1
      if (isNameExpression(tokens(state.idx))) {
        node.add(matchNameExpression(tokens))
      }
      else {
        ??? // TODO: it is a bad situation
      }
    }
    if (state.idx < tokens.length && isSymbol(tokens(state.idx), Symbol.EQUALS)) {
      node.add(Symbol.EQUALS, tokens(state.idx))
      state.idx += 1

      if (isExpression(tokens(state.idx))) {
        node.add(matchExpression(tokens))
      } else {
        ??? // TODO: Bad Situation
      }
    }
    node
  }

  def matchVariableDefStart(tokens: Vector[Token], state: State): MySyntaxNode = {
    var node = MySyntaxNode(BAD)
    tokens(state.idx) match
      case keyword: KeywordToken => keyword.keyword match
        case VAR | VAL => node = MySyntaxNode(keyword.keyword, tokens(state.idx))
      case _ =>  // TODO: Think about it ???
    state.idx += 1
    node
  }

  def matchTypeParamDef(tokens: Vector[Token]): MySyntaxNode = {
    val node = MySyntaxNode(TYPE_PARAMETER_DEFINITION)
    node.add(IDENTIFIER, tokens(state.idx)) // TODO: Check
    state.idx += 1
    if (state.idx < tokens.length && isTypeBound(tokens(state.idx))) {
      node.add(matchTypeBound(tokens, state))
    } else {
      // всё ок
    }
    node
  }


  def matchParamDef(tokens: Vector[Token]): MySyntaxNode = {
    val node = MySyntaxNode(PARAMETER_DEFINITION)
    if (isIdentifier(tokens(state.idx))) {
      node.add(IDENTIFIER, tokens(state.idx))
      state.idx += 1
    }
    else {
      ???
    }
    if (isSymbol(tokens(state.idx), Symbol.COLON)) {
      node.add(Symbol.COLON, tokens(state.idx))
      state.idx += 1
    }
    else {
      ???
    }
    if (isNameExpression(tokens(state.idx))) {
      node.add(matchNameExpression(tokens))
    }
    node
  }

  def matchStatement(tokens: Vector[Token]): MySyntaxNode = {
    tokens(state.idx) match
      case variable if isVariableDefStart(variable) =>
        val node = MySyntaxNode(VARIABLE_DEFINITION_STATEMENT)
        node.add(matchVariableDef(tokens))
        node
      case primary if isPrimary(primary) =>
        val res = matchPrimary(tokens)
        var node = MySyntaxNode()
        if (state.idx < tokens.length && isSymbol(tokens(state.idx), Symbol.EQUALS)) {
          node = MySyntaxNode(ASSIGNMENT_STATEMENT)
          node.add(res)
          node.add(EQUALS, tokens(state.idx))
          state.idx += 1
          if (isExpression(tokens(state.idx))) {
            node.add(matchExpression(tokens))
          } else {
            // хуйня
          }
        } else {
          node = MySyntaxNode(EXPRESSION_STATEMENT)
          node.add(res)
        }
        node
      case expression if isExpression(expression)  =>
        val node = MySyntaxNode(EXPRESSION_STATEMENT)
        node.add(matchExpression(tokens))
        node
      case keyword: KeywordToken => keyword.keyword match
        case RETURN =>
          val node = MySyntaxNode(RETURN_STATEMENT)
          node.add(RETURN, tokens(state.idx))
          state.idx += 1
          if (state.idx < tokens.length && isExpression(tokens(state.idx))) {
            node.add(matchExpression(tokens))
          }
          node
        case BREAK =>
          val node = MySyntaxNode(BREAK_STATEMENT)
          node.add(BREAK, tokens(state.idx))
          state.idx += 1
          node
        case CONTINUE =>
          val node = MySyntaxNode(CONTINUE_STATEMENT)
          node.add(CONTINUE, tokens(state.idx))
          state.idx += 1
          node
        case IF =>
          val node = MySyntaxNode(IF_STATEMENT)
          node.add(IF, tokens(state.idx))
          state.idx += 1
          if (isExpression(tokens(state.idx))) {
            node.add(matchExpression(tokens))
          }
          else {
            // No expr after if
          }
          if (isIndent(tokens(state.idx))) {
            node.add(INDENT, tokens(state.idx))
            val list = MySyntaxNode(LIST)
            state.idx += 1
            while (isStatement(tokens(state.idx))) {
              list.add(matchStatement(tokens))
            }
            node.add(list)
            if (isDedent(tokens(state.idx))) {
              node.add(DEDENT, tokens(state.idx))
              state.idx += 1
            } else {
              ??? // There is no Dedent but is Indent
            }
          } else {
            // No statement_block
          }
          if (isKeyword(tokens(state.idx), ELSE)) {
            node.add(ELSE, tokens(state.idx))
            state.idx += 1
            if (isIndent(tokens(state.idx))) {
              node.add(INDENT, tokens(state.idx))
              state.idx += 1
            } else {
              // No statement_block after else
            }
            
            val list = MySyntaxNode(LIST)
            while (isStatement(tokens(state.idx))) {
              list.add(matchStatement(tokens))
            }
            node.add(list)
            if (isDedent(tokens(state.idx))) {
              node.add(DEDENT, tokens(state.idx))
              state.idx += 1
            } else {
              // NO Dedent (((
            }
          }
          node
        case WHILE =>
          val node = MySyntaxNode(WHILE_STATEMENT)
          node.add(WHILE, tokens(state.idx))
          state.idx += 1
          if (isExpression(tokens(state.idx))) {
            node.add(matchExpression(tokens))
          }
          else {
            // No expr for while loop
          }
          if (isIndent(tokens(state.idx))) {
            node.add(INDENT, tokens(state.idx))
            state.idx += 1
            val list = MySyntaxNode(LIST)
            while (isStatement(tokens(state.idx))) {
              list.add(matchStatement(tokens))
            }
            node.add(list)
            if (isDedent(tokens(state.idx))) {
              node.add(DEDENT, tokens(state.idx))
              state.idx += 1
            } else {
              ??? // There is no Dedent but is Indent
            }
          }
          node
        case FOR =>
          val node = MySyntaxNode(FOR_STATEMENT)
          node.add(FOR, tokens(state.idx))
          state.idx += 1
          if (isPrimary(tokens(state.idx))) {
            node.add(matchPrimary(tokens))
          } else {
            // BAD Situation
          }

          if (isKeyword(tokens(state.idx), IN)) {
            node.add(IN, tokens(state.idx))
            state.idx += 1
          } else {
            // Bad
          }
          if (isExpression(tokens(state.idx))) {
            node.add(matchExpression(tokens))
          }
          if (isIndent(tokens(state.idx))) {
            node.add(INDENT, tokens(state.idx))
            state.idx += 1
            val list = MySyntaxNode(LIST)
            while (isStatement(tokens(state.idx))) {
              list.add(matchStatement(tokens))
            }
            node.add(list)
            if (isDedent(tokens(state.idx))) {
              node.add(DEDENT, tokens(state.idx))
              state.idx += 1
            } else {
              ??? // There is no Dedent but is Indent
            }
          }
          node
        case _ => MySyntaxNode(BAD, tokens(state.idx))
      case _  => MySyntaxNode(BAD, tokens(state.idx))
  }


  def matchExpression(tokens: Vector[Token]): MySyntaxNode = {
    val node = MySyntaxNode(BAD)
    tokens(state.idx) match
      case nameExpr if isNameExpression(nameExpr) => return matchNameExpression(tokens)
      case primaryExpr if isPrimary(primaryExpr) => return node
      case expr => return node
  }

  def matchPrimary(tokens: Vector[Token]): MySyntaxNode = {
    tokens(state.idx) match
      case nameExpr if isNameExpression(nameExpr) => matchNameExpression(tokens)
      case startOfDefaultPrimary => MySyntaxNode(BAD) // ???
  }

  def matchNameExpression(tokens: Vector[Token]): MySyntaxNode = {
    tokens(state.idx) match
      case i: IdentifierToken =>
        if (state.idx + 1 < tokens.length && isSymbol(tokens(state.idx + 1), Symbol.LESS_THAN)) matchGenericNameExpression(tokens)
        else matchIdentifierNameExpression(tokens)
      case q: SymbolToken if isSymbol(q, Symbol.QUESTION) => matchOptionNameExpression(tokens)

  }

  def matchIdentifierNameExpression(tokens: Vector[Token]): MySyntaxNode = {
    val node = MySyntaxNode(IDENTIFIER_NAME_EXPRESSION)
    node.add(IDENTIFIER, tokens(state.idx))
    state.idx += 1
    node
  }


  def matchOptionNameExpression(tokens: Vector[Token]): MySyntaxNode = {
    val node = MySyntaxNode(OPTION_NAME_EXPRESSION)
    node.add(Symbol.QUESTION, tokens(state.idx))
    state.idx += 1
    node.add(matchNameExpression(tokens))
    node
  }

  def matchGenericNameExpression(tokens: Vector[Token]): MySyntaxNode = {
    val node = MySyntaxNode(GENERIC_NAME_EXPRESSION)
    node.add(IDENTIFIER, tokens(state.idx))
    state.idx += 1
    if (isSymbol(tokens(state.idx), Symbol.LESS_THAN)) {
      node.add(Symbol.LESS_THAN, tokens(state.idx))
      state.idx += 1
      val sepList = MySyntaxNode(SEPARATED_LIST)
      if (isNameExpression(tokens(state.idx))) {
        sepList.add(matchNameExpression(tokens))
      } else {
        ???
      }

      while (isSymbol(tokens(state.idx), Symbol.COMMA)) {
        sepList.add(Symbol.COMMA, tokens(state.idx))
        state.idx += 1
        if (isNameExpression(tokens(state.idx))) {
          sepList.add(matchNameExpression(tokens))
        } else {
          ???
        }
      }
      node.add(sepList)
      if (isSymbol(tokens(state.idx), Symbol.GREATER_THAN)) {
        node.add(Symbol.GREATER_THAN, tokens(state.idx))
        state.idx += 1
      }
    }
    node
  }


  override def parse(s: String): ParseResult = {
    val lexer = Tokenizer()
    val tokens: Vector[Token] = lexer.lex(s).asScala.toVector
    val parseResult = MyParseResult(SOURCE_TEXT)
    parseResult.addToRoot(matchTypeDef(tokens))
    parseResult
  }
}
