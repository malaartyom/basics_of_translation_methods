package ParserImplementation

import LexerImplementation.Tokenizer
import ParserImplementation.Checkers.*
import ParserImplementation.MyParser.priority
import syspro.tm.lexer.Keyword.*
import syspro.tm.lexer.Symbol.*
import syspro.tm.lexer.{BadToken, BooleanLiteralToken, IdentifierToken, IntegerLiteralToken, Keyword, KeywordToken, RuneLiteralToken, StringLiteralToken, Symbol, SymbolToken, Token}
import syspro.tm.parser.SyntaxKind.*
import syspro.tm.parser.{ParseResult, Parser}

import scala.util.boundary
import scala.Predef.{???, *}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

case class MyParser() extends Parser {

  var state = State()
  var parseResult = MyParseResult(SOURCE_TEXT)

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
      if (sepList.slotCount() == 0) {
        node.add(null)
      }
      else {
        node.add(sepList)
      }
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
      val token = tokens(state.idx)
      val newToken = KeywordToken(token.start, token.end, token.leadingTriviaLength, token.trailingTriviaLength, token.asInstanceOf[IdentifierToken].contextualKeyword)
      node.add(terminal, newToken) // TODO: Func in state
      state.idx += 1
    } else {
      println(tokens(state.idx))
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
            // TODO: что-то плохое надо пропустить запятую и дальше парсить
          }
        }
        if (sepList.slotCount() == 0) {
          node.add(null)
        }
        else {
          node.add(sepList)
        }
      }

      if (isSymbol(tokens(state.idx), Symbol.GREATER_THAN)) {
        node.add(Symbol.GREATER_THAN, tokens(state.idx))
        state.idx += 1
      }
    } else {
      node.addFail(3)
    }
    if (isTypeBound(tokens(state.idx))) {
      node.add(matchTypeBound(tokens, state))
      state.drop()
    } else {
      node.addFail(1)
    }
    if (state.idx < tokens.length && isIndent(tokens(state.idx))) { // TODO: Do same thing with other conditions
      node.add(INDENT, tokens(state.idx))
      state.indentLevel += 1
      state.idx += 1
      val thisIndentLevel = state.indentLevel
      val list = MySyntaxNode(LIST)
      while (isDefinition(tokens(state.idx))) {
        list.add(matchDefinition(tokens))
      }
      if (list.slotCount() == 0) {
        node.add(null)
      }
      else {
        node.add(list)
      }
      if (isDedent(tokens(state.idx))) {
        node.add(DEDENT, tokens(state.idx))
        state.idx += 1
        state.indentLevel -= 1
      } else {
        boundary:
          while (!isDedent(tokens(state.idx)) || state.indentLevel != thisIndentLevel) {
            if (isIndent(tokens(state.idx))) {
              state.indentLevel += 1
            } else if (isDedent(tokens(state.idx))) {
              state.indentLevel -= 1
            }
            state.idx += 1
          }
        node.add(DEDENT, tokens(state.idx))
        state.indentLevel -= 1
        // Нет Dedenta но есть Indent очеьн плохо
      }
    } else {
      node.addFail(3)
      // Empty member block
    }
    node
  }

  def matchTypeDefStart(tokens: Vector[Token]): Keyword = tokens(state.idx).asInstanceOf[IdentifierToken].contextualKeyword

  def matchFuncDef(tokens: Vector[Token]): MySyntaxNode = {
    val node = MySyntaxNode(FUNCTION_DEFINITION)
    val list = MySyntaxNode(LIST)
    while (isFunctionDefStart(tokens(state.idx)) && !isKeyword(tokens(state.idx), DEF)) {
      list.add(matchFuncDefStart(tokens))
    }
    if (list.slotCount() == 0) {
      node.add(null)
    }
    else {
      node.add(list)
    }
    if (isKeyword(tokens(state.idx), DEF)) {
      node.add(DEF, tokens(state.idx))
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
      if (sepList.slotCount() == 0) {
        node.add(null)
      }
      else {
        node.add(sepList)
      }
    } else {
      node.add(null) // TODO: Think about it ???
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
        // Не хватает типа. Короче что-то плохое
      }
    } else {
      node.addFail(2)
    }

    if (isIndent(tokens(state.idx))) {
      node.add(INDENT, tokens(state.idx))
      state.idx += 1
      state.indentLevel += 1
      val thisIndentLevel = state.indentLevel
      val list = MySyntaxNode(LIST)
      while (isStatement(tokens(state.idx))) {
        list.add(matchStatement(tokens))
      }
      if (list.slotCount() == 0) {
        node.add(null)
      }
      else {
        node.add(list)
      }
      if (isDedent(tokens(state.idx))) {
        node.add(DEDENT, tokens(state.idx))
        state.idx += 1
        state.indentLevel -= 1
      } else {
        val start = tokens(state.idx).start
        while (!isDedent(tokens(state.idx)) || state.indentLevel != thisIndentLevel) {
          if (isIndent(tokens(state.idx))) {
            state.indentLevel += 1
          } else if (isDedent(tokens(state.idx))) {
            state.indentLevel -= 1
          }
          state.idx += 1
        }
        val end = tokens(state.idx - 1).end
        parseResult.addInvalidRange(start, end) // TODO Add in state
        node.add(DEDENT, tokens(state.idx))
        state.indentLevel -= 1
        // Нет Dedenta но есть Indent очень плохо
      }
    } else {
      node.addFail(3)
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
    } else {
      node.addFail(2)
    }
    if (state.idx < tokens.length && isSymbol(tokens(state.idx), Symbol.EQUALS)) {
      node.add(Symbol.EQUALS, tokens(state.idx))
      state.idx += 1

      if (isExpression(tokens(state.idx))) {
        node.add(matchExpression(tokens))
      } else {
        while (!isDefinition(tokens(state.idx)) && !isStatement(tokens(state.idx))) {
          node.addFail(1)
          state.idx += 1
        }
        // TODO: Bad Situation
      }
    } else {
      node.addFail(2)
    }

    node
  }

  def matchVariableDefStart(tokens: Vector[Token], state: State): MySyntaxNode = {
    var node = MySyntaxNode(BAD)
    tokens(state.idx) match
      case keyword: KeywordToken => keyword.keyword match
        case VAR | VAL => node = MySyntaxNode(keyword.keyword, tokens(state.idx))
        case _ => node
      case _ => // TODO: Think about it ???
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
      node.addFail(1)
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
            // плохо
          }
        } else {
          node = MySyntaxNode(EXPRESSION_STATEMENT)
          node.add(res)
        }
        node
      case expression if isExpression(expression) =>
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
        case IF => matchIF(tokens)
        case WHILE => matchWhile(tokens)
        case FOR => matchFor(tokens)
        case _ => MySyntaxNode(BAD, tokens(state.idx))
      case _ => MySyntaxNode(BAD, tokens(state.idx))
  }


  private def matchIF(tokens: Vector[Token]) = {
    val node = MySyntaxNode(IF_STATEMENT)
    node.add(IF, tokens(state.idx))
    state.idx += 1
    if (isExpression(tokens(state.idx))) {
      node.add(matchExpression(tokens))
    }
    else {
      node.add(null)
      // No expr after if
    }
    if (isIndent(tokens(state.idx))) {
      node.add(INDENT, tokens(state.idx))
      state.indentLevel += 1
      val thisIndentLevel = state.indentLevel
      val list = MySyntaxNode(LIST)
      state.idx += 1
      while (isStatement(tokens(state.idx))) {
        list.add(matchStatement(tokens))
      }
      if (list.slotCount() == 0) {

        node.add(null)
      }
      else {
        node.add(list)
      }
      if (isDedent(tokens(state.idx))) {
        node.add(DEDENT, tokens(state.idx))
        state.idx += 1
        state.indentLevel -= 1
      } else {
        val start = tokens(state.idx).start
        while (!isDedent(tokens(state.idx)) || state.indentLevel != thisIndentLevel) {
          if (isIndent(tokens(state.idx))) {
            state.indentLevel += 1
          } else if (isDedent(tokens(state.idx))) {
            state.indentLevel -= 1
          }
          state.idx += 1
        }
        val end = tokens(state.idx - 1).end
        parseResult.addInvalidRange(start, end)
        node.add(DEDENT, tokens(state.idx))
        state.indentLevel -= 1
      }
    } else {
      node.addFail(3)
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
      if (list.slotCount() == 0) {
        node.add(null)
      }
      else {
        node.add(list)
      }
      if (isDedent(tokens(state.idx))) {
        node.add(DEDENT, tokens(state.idx))
        state.idx += 1
      } else {
        // NO Dedent (((
      }
    } else {
      node.addFail(4)
    }
    node
  }

  private def matchWhile(tokens: Vector[Token]) = {
    val node = MySyntaxNode(WHILE_STATEMENT)
    node.add(WHILE, tokens(state.idx))
    state.idx += 1
    if (isExpression(tokens(state.idx))) {
      node.add(matchExpression(tokens))
    }
    else {
      // No expr for while loop
    }
    if (state.idx < tokens.length && isIndent(tokens(state.idx))) {
      node.add(INDENT, tokens(state.idx))
      state.idx += 1
      val list = MySyntaxNode(LIST)
      while (isStatement(tokens(state.idx))) {
        list.add(matchStatement(tokens))
      }
      if (list.slotCount() == 0) {
        node.add(null)
      }
      else {
        node.add(list)
      }
      if (isDedent(tokens(state.idx))) {
        node.add(DEDENT, tokens(state.idx))
        state.idx += 1
      } else {
        ??? // There is no Dedent but is Indent
      }
    }
    else node.addFail(3)
    node
  }

  private def matchFor(tokens: Vector[Token]) = {
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
    if (state.idx < tokens.length && isIndent(tokens(state.idx))) {
      node.add(INDENT, tokens(state.idx))
      state.idx += 1
      val list = MySyntaxNode(LIST)
      while (isStatement(tokens(state.idx))) {
        list.add(matchStatement(tokens))
      }
      if (list.slotCount() == 0) {
        node.add(null)
      }
      else {
        node.add(list)
      }
      if (isDedent(tokens(state.idx))) {
        node.add(DEDENT, tokens(state.idx))
        state.idx += 1
      } else {
        ??? // There is no Dedent but is Indent
      }
    } else {
      node.addFail(3)
    }
    node
  }

  def matchExpression(tokens: Vector[Token]): MySyntaxNode = {
    val opStack = mutable.Stack[SymbolToken | KeywordToken]()
    val nodeStack = mutable.Stack[MySyntaxNode]()

    if (isUnary(tokens(state.idx))) {
      val operation = matchUnary(tokens)
      val node = matchPrimary(tokens)
      operation.add(node)
      nodeStack.push(operation)
      // -this.x + 42 * 13
      // a is b x
    } else if (isPrimary(tokens(state.idx))) nodeStack.push(matchPrimary(tokens))
    while (state.idx < tokens.length && isExpressionContinue(tokens(state.idx))) {
      // TODO: Remove pattern-matching
      val currentPriority = {
        tokens(state.idx) match
          case symbol: SymbolToken => priority(symbol.symbol)
          case keyword: KeywordToken => priority(keyword.keyword)
      }

      val stackPriority = if (opStack.isEmpty) 0 else {
        opStack.top match
          case symbol: SymbolToken => priority(symbol.symbol)
          case keyword: KeywordToken => priority(keyword.keyword)
      }
      if (stackPriority <= currentPriority || opStack.isEmpty) {
        tokens(state.idx) match
          case symbol: SymbolToken => opStack.push(symbol)
          case keyword: KeywordToken => opStack.push(keyword)
      } else {
        // this.x * 12 + 30
        val node = createOpNode(opStack.pop())
        node.add(nodeStack.pop())
        node.addLeft(nodeStack.pop()) // TODO: addLeft
        nodeStack.push(node)
        tokens(state.idx) match
          case symbol: SymbolToken => opStack.push(symbol)
          case keyword: KeywordToken => opStack.push(keyword)
      }
      state.idx += 1
      nodeStack.push(matchPrimary(tokens))
    }

    while (opStack.nonEmpty) {
      val node = createOpNode(opStack.pop())
      node.add(nodeStack.pop())
      node.addLeft(nodeStack.pop())
      nodeStack.push(node)
    }
    nodeStack.pop()
  }

  def matchUnary(tokens: Vector[Token]): MySyntaxNode = {
    var node = MySyntaxNode()
    tokens(state.idx) match
      case symbol: SymbolToken if isUnary(tokens(state.idx)) =>
        symbol.symbol match
          case PLUS => node = MySyntaxNode(UNARY_PLUS_EXPRESSION)
          case MINUS => node = MySyntaxNode(UNARY_MINUS_EXPRESSION)
          case TILDE => node = MySyntaxNode(BITWISE_NOT_EXPRESSION)
          case EXCLAMATION => node = MySyntaxNode(LOGICAL_NOT_EXPRESSION)
        node.add(symbol.symbol, tokens(state.idx))
      case _ => println(s"Not a symbol but matchUnary called! ${tokens(state.idx)}")

    state.idx += 1
    node
  }

  def createOpNode(token: Token): MySyntaxNode = {
    val node: MySyntaxNode = token match
      case symbol: SymbolToken if isSymbol(symbol) =>
        symbol.symbol match
          case PLUS => MySyntaxNode(ADD_EXPRESSION)
          case ASTERISK => MySyntaxNode(MULTIPLY_EXPRESSION)
          case AMPERSAND_AMPERSAND => MySyntaxNode(LOGICAL_AND_EXPRESSION)
          case BAR_BAR => MySyntaxNode(LOGICAL_OR_EXPRESSION)
          case EQUALS_EQUALS => MySyntaxNode(EQUALS_EXPRESSION)
          case EXCLAMATION_EQUALS => MySyntaxNode(NOT_EQUALS_EXPRESSION)
          case LESS_THAN => MySyntaxNode(LESS_THAN_EXPRESSION)
          case LESS_THAN_EQUALS => MySyntaxNode(LESS_THAN_OR_EQUAL_EXPRESSION)
          case GREATER_THAN => MySyntaxNode(GREATER_THAN_EXPRESSION)
          case GREATER_THAN_EQUALS => MySyntaxNode(GREATER_THAN_OR_EQUAL_EXPRESSION)
          case AMPERSAND => MySyntaxNode(BITWISE_AND_EXPRESSION)
          case BAR => MySyntaxNode(BITWISE_OR_EXPRESSION)
          case CARET => MySyntaxNode(BITWISE_EXCLUSIVE_OR_EXPRESSION)
          case LESS_THAN_LESS_THAN => MySyntaxNode(BITWISE_LEFT_SHIFT_EXPRESSION)
          case GREATER_THAN_GREATER_THAN => MySyntaxNode(BITWISE_RIGHT_SHIFT_EXPRESSION)
          case MINUS => MySyntaxNode(SUBTRACT_EXPRESSION)
          case SLASH => MySyntaxNode(DIVIDE_EXPRESSION)
          case PERCENT => MySyntaxNode(MODULO_EXPRESSION)

      case keyword: KeywordToken if isKeyword(keyword, IS) => MySyntaxNode(IS_EXPRESSION)

    node.add(token.asInstanceOf[SymbolToken].symbol, token)
    node
  }

  def matchPrimary(tokens: Vector[Token]): MySyntaxNode = {
    matchDefaultPrimary(tokens)
  }

  private def matchDefaultPrimary(tokens: Vector[Token]): MySyntaxNode = {
    var node: MySyntaxNode = MySyntaxNode()
    var first = true
    while (state.idx < tokens.length && isContinueOfPrimary(tokens(state.idx)) || (first && isPrimary(tokens(state.idx))))
      tokens(state.idx) match
        case bad: BadToken => node =MySyntaxNode(BAD, bad)
          parseResult.addInvalidRange(tokens(state.idx).start, tokens(state.idx).end + 1)
          state.idx += 1
        case nameExpr if isNameExpression(nameExpr) => node = matchNameExpression(tokens)
        case identifier: IdentifierToken if identifier.contextualKeyword != null && identifier.contextualKeyword == NULL =>
          node = MySyntaxNode(NULL_LITERAL_EXPRESSION); node.add(NULL, identifier); state.idx += 1
        case keyword: KeywordToken => keyword.keyword match
          case THIS => node = MySyntaxNode(THIS_EXPRESSION); node.add(THIS, keyword); state.idx += 1
          case SUPER => node = MySyntaxNode(SUPER_EXPRESSION); node.add(SUPER, keyword); state.idx += 1
          case NULL => MySyntaxNode(NULL_LITERAL_EXPRESSION); node.add(NULL, keyword); state.idx += 1
        case rune: RuneLiteralToken => node = MySyntaxNode(RUNE_LITERAL_EXPRESSION); node.add(RUNE, rune); state.idx += 1
        case int: IntegerLiteralToken => node = MySyntaxNode(INTEGER_LITERAL_EXPRESSION); node.add(INTEGER, int); state.idx += 1
        case str: StringLiteralToken => node = MySyntaxNode(STRING_LITERAL_EXPRESSION); node.add(STRING, str); state.idx += 1
        case True: BooleanLiteralToken if True.value => node = MySyntaxNode(TRUE_LITERAL_EXPRESSION); node.add(BOOLEAN, True); state.idx += 1
        case False: BooleanLiteralToken if !False.value => node = MySyntaxNode(FALSE_LITERAL_EXPRESSION); node.add(BOOLEAN, False); state.idx += 1
        case symbolToken: SymbolToken => symbolToken.symbol match {
          case DOT =>
            val dotNode = MySyntaxNode(MEMBER_ACCESS_EXPRESSION)
            dotNode.add(node)
            dotNode.add(DOT, tokens(state.idx))
            state.idx += 1
            if (state.idx < tokens.length && isIdentifier(tokens(state.idx))) {
              dotNode.add(IDENTIFIER, tokens(state.idx))
              state.idx += 1
            } else {
              // Нету идентификатора после точки
            }
            node = dotNode
          case OPEN_BRACKET =>
            val bracketNode = MySyntaxNode(INDEX_EXPRESSION)
            bracketNode.add(node)
            bracketNode.add(OPEN_BRACKET, tokens(state.idx))
            state.idx += 1
            if (isExpression(tokens(state.idx))) {
              bracketNode.add(matchExpression(tokens))
            }
            if (isSymbol(tokens(state.idx), CLOSE_BRACKET)) {
              bracketNode.add(CLOSE_BRACKET, tokens(state.idx))
              state.idx += 1
            }
            node = bracketNode
          case OPEN_PAREN if first =>

            val parenNode = MySyntaxNode(PARENTHESIZED_EXPRESSION)
            parenNode.add(OPEN_PAREN, symbolToken)
            state.idx += 1
            parenNode.add(matchExpression(tokens))
            if (isSymbol(tokens(state.idx), CLOSE_PAREN)) {
              parenNode.add(CLOSE_PAREN, tokens(state.idx))
              state.idx += 1
            }
            node = parenNode
          case OPEN_PAREN if !first =>
            val invocationNode = MySyntaxNode(INVOCATION_EXPRESSION)
            invocationNode.add(node)
            invocationNode.add(OPEN_PAREN, tokens(state.idx))
            state.idx += 1
            val sepList = MySyntaxNode(SEPARATED_LIST)
            if (isExpression(tokens(state.idx))) {
              sepList.add(matchExpression(tokens))
              while (isSymbol(tokens(state.idx), COMMA)) {
                sepList.add(COMMA, tokens(state.idx))
                state.idx += 1
                if (isExpression(tokens(state.idx))) {
                  sepList.add(matchExpression(tokens))
                }
              }
            } else {
              // пустой sepList
            }
            if (sepList.slotCount() == 0) {
              invocationNode.add(null)
            }
            else {
                invocationNode.add(sepList)
            }
            if (isSymbol(tokens(state.idx), CLOSE_PAREN)) {
              invocationNode.add(CLOSE_PAREN, tokens(state.idx))
              state.idx += 1
            }
            node = invocationNode
          case CLOSE_BRACKET => return node
          case CLOSE_PAREN => return node
        }
      first = false
    node
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
    var node = MySyntaxNode(GENERIC_NAME_EXPRESSION)
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
      if (sepList.slotCount() == 0) {
        node.add(null)
      }
      else {
        node.add(sepList)
      }
      if (isSymbol(tokens(state.idx), Symbol.GREATER_THAN)) {
        node.add(Symbol.GREATER_THAN, tokens(state.idx))
        state.idx += 1
      } else {
        state.idx -= 3
        node = matchIdentifierNameExpression(tokens)
      }
    }
    node
  }


  override def parse(s: String): ParseResult = {
    state.idx = 0
    state.indentLevel = 0
    val lexer = Tokenizer()
    val tokens: Vector[Token] = lexer.lex(s).asScala.toVector
    parseResult = MyParseResult(SOURCE_TEXT)
    val list = MySyntaxNode(LIST)
    boundary:
      while (state.idx < tokens.length) {
        if (isTypeDefStart(tokens(state.idx))) {
          list.add(matchTypeDef(tokens))
        } else {
          while (state.idx < tokens.length && !isTypeDefStart(tokens(state.idx))) {
            state.idx += 1
          }
        }
      }
    if (list.slotCount() == 0) {parseResult.addToRoot(null)}
    else {parseResult.addToRoot(list)}
    parseResult
  }
}


case object MyParser {
  val priority: Map[Symbol | Keyword, Int] = Map(
    ASTERISK -> 2,
    SLASH -> 2,
    PERCENT -> 2,

    PLUS -> 3,
    MINUS -> 3,

    LESS_THAN_LESS_THAN -> 4,
    GREATER_THAN_GREATER_THAN -> 4,

    AMPERSAND -> 5,
    CARET -> 6,
    BAR -> 7,

    LESS_THAN -> 8,
    GREATER_THAN -> 8,
    LESS_THAN_EQUALS -> 8,
    GREATER_THAN_EQUALS -> 8,
    EQUALS_EQUALS -> 8,
    EXCLAMATION_EQUALS -> 8,
    IS -> 8,

    AMPERSAND_AMPERSAND -> 10,
    BAR_BAR -> 11
  )

}