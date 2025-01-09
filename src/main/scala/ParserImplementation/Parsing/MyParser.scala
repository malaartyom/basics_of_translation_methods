package ParserImplementation.Parsing

import LexerImplementation.Tokenizer
import ParserImplementation.Utils.Checkers.*
import MyParser.priority
import ParserImplementation.Utils.State
import syspro.tm.lexer.Keyword.*
import syspro.tm.lexer.Symbol.*
import syspro.tm.lexer.{BadToken, BooleanLiteralToken, IdentifierToken, IntegerLiteralToken, Keyword, KeywordToken, RuneLiteralToken, StringLiteralToken, Symbol, SymbolToken, Token}
import syspro.tm.parser.SyntaxKind.*
import syspro.tm.parser.{ParseResult, Parser}

import scala.Predef.{???, *}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.boundary

case class MyParser() extends Parser {

  var state = State()
  var parseResult = MyParseResult(SOURCE_TEXT)
  var tokens = Vector[Token]()

  def matchTypeBound(): MySyntaxNode = {
    val node = MySyntaxNode(TYPE_BOUND)
    node.add(Symbol.BOUND, tokens(state.idx)) // TODO: Check if not BOUND Same in all other match* functions
    state.idx += 1

    if (isNameExpression(tokens(state.idx))) {

      val sepList = MySyntaxNode(SEPARATED_LIST)

      sepList.add(matchNameExpression())

      while (state.idx < tokens.length && isSymbol(tokens(state.idx), Symbol.AMPERSAND)) {
        sepList.add(Symbol.AMPERSAND, tokens(state.idx))
        state.idx += 1

        if (isNameExpression(tokens(state.idx))) {
          sepList.add(matchNameExpression())
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

  def matchDefinition(): MySyntaxNode = {
    tokens(state.idx) match
      case typeDef: IdentifierToken if isTypeDefStart(typeDef) => matchTypeDef()
      case funcDef: KeywordToken if isFunctionDefStart(funcDef) => matchFuncDef()
      case varDef: KeywordToken if isVariableDefStart(varDef) => matchVariableDef()
      case typeParameterDef: IdentifierToken if isIdentifier(typeParameterDef) => matchTypeParamDef()
      case parameterDef: IdentifierToken if isIdentifier(parameterDef) && state.idx + 1 < tokens.length
        && isSymbol(tokens(state.idx + 1), Symbol.COLON) => matchParamDef()
  }

  def matchTypeDef(): MySyntaxNode = {
    val node = MySyntaxNode(TYPE_DEFINITION)

    if (isTypeDefStart(tokens(state.idx))) {
      val terminal = matchTypeDefStart()
      val token = tokens(state.idx)
      val newToken = KeywordToken(token.start, token.end, token.leadingTriviaLength, token.trailingTriviaLength, token.asInstanceOf[IdentifierToken].contextualKeyword)
      node.add(terminal, newToken) // TODO: Func in state
      state.idx += 1
    } else {
      node.addFail(1)
      parseResult.addInvalidRange(tokens(state.idx).fullSpan())
      parseResult.addDiagnostic(tokens(state.idx).fullSpan(), 0)
      state.idx += 1
    }

    if (isIdentifier(tokens(state.idx))) {
      node.add(IDENTIFIER, tokens(state.idx))
      state.idx += 1
    } else {
      node.addFail(1)
      parseResult.addInvalidRange(tokens(state.idx).fullSpan())
      parseResult.addDiagnostic(tokens(state.idx).fullSpan(), 1)
      state.idx += 1
    }

    if (isSymbol(tokens(state.idx), Symbol.LESS_THAN)) {
      node.add(Symbol.LESS_THAN, tokens(state.idx))
      state.idx += 1

      if (isTypeParameterDef(tokens(state.idx))) {
        val sepList = MySyntaxNode(SEPARATED_LIST)
        sepList.add(matchTypeParamDef())

        while (isSymbol(tokens(state.idx), Symbol.COMMA)) {
          sepList.add(Symbol.COMMA, tokens(state.idx))
          state.idx += 1
          if (isTypeParameterDef(tokens(state.idx))) {
            sepList.add(matchTypeParamDef())
          } else {
            while (!isTypeParameterDef(tokens(state.idx))) {
              sepList.addFail(1)
              parseResult.addInvalidRange(tokens(state.idx).fullSpan())
              parseResult.addDiagnostic(tokens(state.idx).fullSpan(), 2)
              state.idx += 1
            }
            // TODO: добавить в качестве отдельной фунции в стейт
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
      node.add(matchTypeBound())
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
        list.add(matchDefinition())
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
        while (!isDedent(tokens(state.idx)) || state.indentLevel != thisIndentLevel) {
          if (isIndent(tokens(state.idx))) {
            state.indentLevel += 1
          } else if (isDedent(tokens(state.idx))) {
            state.indentLevel -= 1
          }
          parseResult.addInvalidRange(tokens(state.idx).fullSpan())
          parseResult.addDiagnostic(tokens(state.idx).fullSpan(), 7) // TODO: Func
          state.idx += 1
        }
        node.add(DEDENT, tokens(state.idx))
        state.indentLevel -= 1
        // Нет Dedenta но есть Indent очеьн плохо
      }
    } else {
      while (state.idx < tokens.length && !isTypeDefStart(tokens(state.idx))) {
        parseResult.addInvalidRange(tokens(state.idx).fullSpan())
        parseResult.addDiagnostic(tokens(state.idx).fullSpan(), 7) // TODO: Func
        state.idx += 1
      }
      node.addFail(3)
      // Empty member block
    }
    node
  }

  def matchTypeDefStart(): Keyword = tokens(state.idx).asInstanceOf[IdentifierToken].contextualKeyword

  def matchFuncDef(): MySyntaxNode = {
    val node = MySyntaxNode(FUNCTION_DEFINITION)
    val list = MySyntaxNode(LIST)
    while (isFunctionDefStart(tokens(state.idx)) && !isKeyword(tokens(state.idx), DEF)) {
      list.add(matchFuncDefStart())
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
      sepList.add(matchParamDef())
      while (isSymbol(tokens(state.idx), COMMA)) {
        sepList.add(COMMA, tokens(state.idx))
        state.idx += 1
        if (isIdentifier(tokens(state.idx))) {
          sepList.add(matchParamDef())
        }
        else {
          // Есть запятая но нет дальше ничего
        }
      }
      if (sepList.slotCount() == 0) {
        node.addFail(1)
      }
      else {
        node.add(sepList)
      }
    } else {
      node.addFail(1)
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
        node.add(matchNameExpression())
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
        list.add(matchStatement())
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
        skipUntilDedent(node, thisIndentLevel)
        // Нет Dedenta но есть Indent очень плохо
      }
    } else {
      node.addFail(3)
    }
    node
  }

  def matchFuncDefStart(): MySyntaxNode = {
    var node: MySyntaxNode = null
    tokens(state.idx) match
      case keyword: KeywordToken =>
        keyword.keyword match
          case ABSTRACT | VIRTUAL | OVERRIDE | NATIVE => node = MySyntaxNode(keyword.keyword, tokens(state.idx));
      case _ =>
    state.idx += 1
    if (node == null) {
      node = MySyntaxNode(BAD, tokens(state.idx))
    }
    node
  }

  def matchVariableDef(): MySyntaxNode = {
    val node = MySyntaxNode(VARIABLE_DEFINITION)
    if (isVariableDefStart(tokens(state.idx))) {
      node.add(matchVariableDefStart())
    } else {
      node.addFail(1)
      parseResult.addInvalidRange(tokens(state.idx).fullSpan())
      parseResult.addDiagnostic(tokens(state.idx).fullSpan(), 3)
      state.idx += 1
      // TODO: добавить в качестве отдельной фунции в стейт
    }
    if (isIdentifier(tokens(state.idx))) {
      node.add(IDENTIFIER, tokens(state.idx))
      state.idx += 1
    } else {
      node.addFail(1)
      parseResult.addInvalidRange(tokens(state.idx).fullSpan())
      parseResult.addDiagnostic(tokens(state.idx).fullSpan(), 1)
      state.idx += 1
    }
    if (state.idx < tokens.length && isSymbol(tokens(state.idx), Symbol.COLON)) {
      node.add(Symbol.COLON, tokens(state.idx))
      state.idx += 1
      if (isNameExpression(tokens(state.idx))) {
        node.add(matchNameExpression())
      }
      else {
        while (!isDefinition(tokens(state.idx))) {
          node.addFail(1)
          parseResult.addInvalidRange(tokens(state.idx).fullSpan()) // TODO: In function
          parseResult.addDiagnostic(tokens(state.idx).fullSpan(), 4)
          state.idx += 1
        }
      }
    } else {
      node.addFail(2)
    }
    if (state.idx < tokens.length && isSymbol(tokens(state.idx), Symbol.EQUALS)) {
      node.add(Symbol.EQUALS, tokens(state.idx))
      state.idx += 1

      if (isExpression(tokens(state.idx))) {
        node.add(matchExpression())
      } else {
        while (!isDefinition(tokens(state.idx)) && !isStatement(tokens(state.idx))) {
          node.addFail(1)
          parseResult.addInvalidRange(tokens(state.idx).fullSpan()) // TODO: In function
          parseResult.addDiagnostic(tokens(state.idx).fullSpan(), 6)
          state.idx += 1
        }
        // TODO: Bad Situation
      }
    } else {
      node.addFail(2)
    }

    node
  }

  def matchVariableDefStart(): MySyntaxNode = {
    var node: MySyntaxNode = null
    tokens(state.idx) match
      case keyword: KeywordToken => keyword.keyword match
        case VAR | VAL => node = MySyntaxNode(keyword.keyword, tokens(state.idx))
        case _ => node = MySyntaxNode(BAD, tokens(state.idx))
      case _ => parseResult.addInvalidRange(tokens(state.idx).fullSpan()); node = MySyntaxNode(BAD, tokens(state.idx)) // TODO: Think about it
    state.idx += 1
    if (node == null) {
      node = MySyntaxNode(BAD, tokens(state.idx))
    }
    node
  }

  def matchTypeParamDef(): MySyntaxNode = {
    val node = MySyntaxNode(TYPE_PARAMETER_DEFINITION)
    node.add(IDENTIFIER, tokens(state.idx)) // TODO: Check
    state.idx += 1
    if (state.idx < tokens.length && isTypeBound(tokens(state.idx))) {
      node.add(matchTypeBound())
    } else {
      node.addFail(1)
      // всё ок
    }
    node
  }


  def matchParamDef(): MySyntaxNode = {
    val node = MySyntaxNode(PARAMETER_DEFINITION)
    if (isIdentifier(tokens(state.idx))) {
      node.add(IDENTIFIER, tokens(state.idx))
      state.idx += 1
    }
    else {
      node.addFail(1)
      parseResult.addInvalidRange(tokens(state.idx).fullSpan())
      parseResult.addDiagnostic(tokens(state.idx).fullSpan(), 1)
      state.idx += 1
    }
    if (isSymbol(tokens(state.idx), Symbol.COLON)) {
      node.add(Symbol.COLON, tokens(state.idx))
      state.idx += 1
    }
    else {
      // Everything is ok
    }
    if (isNameExpression(tokens(state.idx))) {
      node.add(matchNameExpression())
    }
    node
  }

  def matchStatement(): MySyntaxNode = {
    tokens(state.idx) match
      case variable if isVariableDefStart(variable) =>
        val node = MySyntaxNode(VARIABLE_DEFINITION_STATEMENT)
        node.add(matchVariableDef())
        node
      case primary if isPrimary(primary) =>
        val res = matchPrimary()
        var node: MySyntaxNode = null
        if (state.idx < tokens.length && isSymbol(tokens(state.idx), Symbol.EQUALS)) {
          node = MySyntaxNode(ASSIGNMENT_STATEMENT)
          node.add(res)
          node.add(EQUALS, tokens(state.idx))
          state.idx += 1
          if (isExpression(tokens(state.idx))) {
            node.add(matchExpression())
          } else {
            // плохо
          }
        } else if (state.idx < tokens.length && isKeyword(tokens(state.idx), Keyword.IS)) {
          node = MySyntaxNode(EXPRESSION_STATEMENT)
          node.add(IS_EXPRESSION)
          val child = node.children.head.asInstanceOf[MySyntaxNode]
          child.add(res)
          child.add(IS, tokens(state.idx))
          state.idx += 1
          if (isExpression(tokens(state.idx))) {
            child.add(matchExpression())
          } else {
            // плохо
          }
          if (isIdentifier(tokens(state.idx))) {
            child.add(IDENTIFIER, tokens(state.idx))
          } else {
            child.addFail(1)
          }


        } else {
          node = MySyntaxNode(EXPRESSION_STATEMENT)
          node.add(res)
        }
        if (node == null) {
          node = MySyntaxNode(BAD, tokens(state.idx))
        }
        node
      case expression if isExpression(expression) =>
        val node = MySyntaxNode(EXPRESSION_STATEMENT)
        node.add(matchExpression())
        node
      case keyword: KeywordToken => keyword.keyword match
        case RETURN =>
          val node = MySyntaxNode(RETURN_STATEMENT)
          node.add(RETURN, tokens(state.idx))
          state.idx += 1
          if (state.idx < tokens.length && isExpression(tokens(state.idx))) {
            node.add(matchExpression())
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
        case IF => matchIF()
        case WHILE => matchWhile()
        case FOR => matchFor()
        case _ => MySyntaxNode(BAD, tokens(state.idx))
      case _ => MySyntaxNode(BAD, tokens(state.idx))
  }


  private def matchIF() = {
    var node = MySyntaxNode(IF_STATEMENT)
    node.add(IF, tokens(state.idx))
    state.idx += 1
    if (isExpression(tokens(state.idx))) {
      node.add(matchExpression())
    }
    else {
      node.add(null)
      // No expr after if
    }
    if (isIndent(tokens(state.idx))) {
      node.add(INDENT, tokens(state.idx))
      state.indentLevel += 1
      val thisIndentLevel = state.indentLevel // TODO: In state
      val list = MySyntaxNode(LIST)
      state.idx += 1
      while (isStatement(tokens(state.idx))) {
        list.add(matchStatement())
      }
      if (list.slotCount() == 0) {
        val newNode = MySyntaxNode(IF_STATEMENT)
        node.addFail(1)
      }
      else {
        node.add(list)
      }
      if (isDedent(tokens(state.idx))) {
        node.add(DEDENT, tokens(state.idx))
        state.idx += 1
        state.indentLevel -= 1
      } else {
        skipUntilDedent(node, thisIndentLevel)
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
        list.add(matchStatement())
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

  private def skipUntilDedent(node: MySyntaxNode, thisIndentLevel: Int): Unit = { // TODO: MOve in stat
    while (!isDedent(tokens(state.idx)) || state.indentLevel != thisIndentLevel) {
      if (isIndent(tokens(state.idx))) {
        state.indentLevel += 1
      } else if (isDedent(tokens(state.idx))) {
        state.indentLevel -= 1
      }
      parseResult.addInvalidRange(tokens(state.idx).fullSpan())
      parseResult.addDiagnostic(tokens(state.idx).fullSpan(), 7)
      state.idx += 1
    }
    node.add(DEDENT, tokens(state.idx))
    state.indentLevel -= 1
  }

  private def matchWhile() = {
    val node = MySyntaxNode(WHILE_STATEMENT)
    node.add(WHILE, tokens(state.idx))
    state.idx += 1
    if (isExpression(tokens(state.idx))) {
      node.add(matchExpression())
    }
    else {
      // No expr for while loop
    }
    if (state.idx < tokens.length && isIndent(tokens(state.idx))) {
      node.add(INDENT, tokens(state.idx))
      state.indentLevel += 1
      val thisIndentLevel = state.indentLevel
      state.idx += 1
      val list = MySyntaxNode(LIST)
      while (isStatement(tokens(state.idx))) {
        list.add(matchStatement())
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
        skipUntilDedent(node, thisIndentLevel) // There is no Dedent but is Indent
      }
    }
    else node.addFail(3)
    node
  }

  private def matchFor() = {
    val node = MySyntaxNode(FOR_STATEMENT)
    node.add(FOR, tokens(state.idx))
    state.idx += 1
    if (isPrimary(tokens(state.idx))) {
      node.add(matchPrimary())
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
      node.add(matchExpression())
    }
    if (state.idx < tokens.length && isIndent(tokens(state.idx))) {
      node.add(INDENT, tokens(state.idx))
      var thisIndentLevel = state.indentLevel
      state.idx += 1
      val list = MySyntaxNode(LIST)
      while (isStatement(tokens(state.idx))) {
        list.add(matchStatement())
      }
      if (list.slotCount() == 0) {
        node.add(null)
      }
      else {
        node.add(list)
      }
      if (isDedent(tokens(state.idx))) {
        node.add(DEDENT, tokens(state.idx))
        thisIndentLevel -= 1
        state.idx += 1
      } else {
        skipUntilDedent(node, thisIndentLevel) // There is no Dedent but is Indent
      }
    } else {
      node.addFail(3)
    }
    node
  }

  def matchExpression(): MySyntaxNode = {
    val opStack = mutable.Stack[SymbolToken | KeywordToken]()
    val nodeStack = mutable.Stack[MySyntaxNode]()

    if (isUnary(tokens(state.idx))) {
      val operation = matchUnary()
      val node = matchPrimary()
      operation.add(node)
      nodeStack.push(operation)
      // -this.x + 42 * 13
      // a is b x
    } else if (isPrimary(tokens(state.idx)))
      nodeStack.push(matchPrimary())

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
        node.addLeft(nodeStack.pop())
        nodeStack.push(node)
        tokens(state.idx) match
          case symbol: SymbolToken => opStack.push(symbol)
          case keyword: KeywordToken => opStack.push(keyword)
      }
      state.idx += 1
      if (isPrimary(tokens(state.idx))) {
        nodeStack.push(matchPrimary())
      } else {
        parseResult.addInvalidRange(nodeStack.top.fullSpan())
        parseResult.addInvalidRange(opStack.top.fullSpan())
        parseResult.addInvalidRange(tokens(state.idx).fullSpan())
        state.idx += 1
        while (state.idx < tokens.length && !isDefinition(tokens(state.idx)) && !isSymbol(tokens(state.idx), CLOSE_PAREN)) {
          parseResult.addInvalidRange(tokens(state.idx).fullSpan()) // TODO: USe in all cases
          parseResult.addDiagnostic(tokens(state.idx).fullSpan(), 10)
          state.idx += 1
        }
        if (isSymbol(tokens(state.idx), CLOSE_PAREN)) {
          nodeStack.push(matchPrimary())
        }
      }

    }

    while (opStack.nonEmpty) {
      val node = createOpNode(opStack.pop())
      node.add(nodeStack.pop())
      node.addLeft(nodeStack.pop())
      nodeStack.push(node)
    }
    nodeStack.pop()
  }

  def matchUnary(): MySyntaxNode = {
    var node: MySyntaxNode = null
    tokens(state.idx) match
      case symbol: SymbolToken if isUnary(tokens(state.idx)) =>
        symbol.symbol match
          case PLUS => node = MySyntaxNode(UNARY_PLUS_EXPRESSION)
          case MINUS => node = MySyntaxNode(UNARY_MINUS_EXPRESSION)
          case TILDE => node = MySyntaxNode(BITWISE_NOT_EXPRESSION)
          case EXCLAMATION => node = MySyntaxNode(LOGICAL_NOT_EXPRESSION)
        node.add(symbol.symbol, tokens(state.idx))
      case _ => println(s"Not a symbol but matchUnary called! ${tokens(state.idx)}")
    if (node == null) {
      node = MySyntaxNode(BAD, tokens(state.idx))
    }
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

  def matchPrimary(): MySyntaxNode = {
    matchDefaultPrimary()
  }

  private def matchDefaultPrimary(): MySyntaxNode = {
    var node: MySyntaxNode = null
    var first = true
    while (state.idx < tokens.length && (isContinueOfPrimary(tokens(state.idx)) || (first && isPrimary(tokens(state.idx)))))
      tokens(state.idx) match
        case bad: BadToken => node = MySyntaxNode(BAD, bad)
          parseResult.addInvalidRange(bad.fullSpan())
          state.idx += 1
        case nameExpr if isNameExpression(nameExpr) => node = matchNameExpression()
        case identifier: IdentifierToken if identifier.contextualKeyword != null && identifier.contextualKeyword == NULL =>
          node = MySyntaxNode(NULL_LITERAL_EXPRESSION); node.add(NULL, identifier); state.idx += 1
        case keyword: KeywordToken => keyword.keyword match
          case THIS => node = MySyntaxNode(THIS_EXPRESSION); node.add(THIS, keyword); state.idx += 1
          case SUPER => node = MySyntaxNode(SUPER_EXPRESSION); node.add(SUPER, keyword); state.idx += 1
          case NULL => node = MySyntaxNode(NULL_LITERAL_EXPRESSION); node.add(NULL, keyword); state.idx += 1
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
              bracketNode.add(matchExpression())
            }
            if (isSymbol(tokens(state.idx), CLOSE_BRACKET)) {
              bracketNode.add(CLOSE_BRACKET, tokens(state.idx))
              state.idx += 1
            }
            node = bracketNode
          case OPEN_PAREN if first =>
            if (isSymbol(tokens(state.idx + 1), CLOSE_PAREN)) {
              val parenNode = MySyntaxNode(INVOCATION_EXPRESSION)
              parenNode.add(OPEN_PAREN, symbolToken)
              state.idx += 1
              parenNode.add(CLOSE_PAREN, symbolToken)
              return parenNode
            }

            val parenNode = MySyntaxNode(PARENTHESIZED_EXPRESSION)
            parenNode.add(OPEN_PAREN, symbolToken)
            state.idx += 1
            parenNode.add(matchExpression())
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
              sepList.add(matchExpression())
              while (state.idx < tokens.length && isSymbol(tokens(state.idx), COMMA)) {
                sepList.add(COMMA, tokens(state.idx))
                state.idx += 1
                if (isExpression(tokens(state.idx))) {
                  sepList.add(matchExpression())
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
            if (state.idx < tokens.length && isSymbol(tokens(state.idx), CLOSE_PAREN)) {
              invocationNode.add(CLOSE_PAREN, tokens(state.idx))
              state.idx += 1
            }
            node = invocationNode
          case CLOSE_BRACKET => return node
          case CLOSE_PAREN => return node
        }
      first = false
    if (node == null) {
      node = MySyntaxNode(BAD, tokens(state.idx))
    }
    node
  }


  def matchNameExpression(): MySyntaxNode = {
    tokens(state.idx) match
      case i: IdentifierToken =>
        if (state.idx + 1 < tokens.length && isSymbol(tokens(state.idx + 1), Symbol.LESS_THAN)) matchGenericNameExpression()
        else matchIdentifierNameExpression()
      case q: SymbolToken if isSymbol(q, Symbol.QUESTION) => matchOptionNameExpression()

  }

  def matchIdentifierNameExpression(): MySyntaxNode = {
    val node = MySyntaxNode(IDENTIFIER_NAME_EXPRESSION)
    node.add(IDENTIFIER, tokens(state.idx))
    state.idx += 1
    node
  }


  def matchOptionNameExpression(): MySyntaxNode = {
    val node = MySyntaxNode(OPTION_NAME_EXPRESSION)
    node.add(Symbol.QUESTION, tokens(state.idx))
    state.idx += 1
    node.add(matchNameExpression())
    node
  }

  def matchGenericNameExpression(): MySyntaxNode = {
    var node = MySyntaxNode(GENERIC_NAME_EXPRESSION)
    node.add(IDENTIFIER, tokens(state.idx))
    state.idx += 1
    if (isSymbol(tokens(state.idx), Symbol.LESS_THAN)) {
      node.add(Symbol.LESS_THAN, tokens(state.idx))
      state.idx += 1
      val sepList = MySyntaxNode(SEPARATED_LIST)
      if (isNameExpression(tokens(state.idx))) {
        sepList.add(matchNameExpression())
      } else {
        state.idx -= 2
        node = matchIdentifierNameExpression()
        return node
      }

      while (isSymbol(tokens(state.idx), Symbol.COMMA)) {
        sepList.add(Symbol.COMMA, tokens(state.idx))
        state.idx += 1
        if (isNameExpression(tokens(state.idx))) {
          sepList.add(matchNameExpression())
        } else {
          while (!isDefinition(tokens(state.idx))) {
            parseResult.addInvalidRange(tokens(state.idx).fullSpan()) // TODO: In function
            parseResult.addDiagnostic(tokens(state.idx).fullSpan(), 5)
            state.idx += 1
          }
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
      } else if (isSymbol(tokens(state.idx), Symbol.GREATER_THAN_GREATER_THAN)) {
        tokens = state.doubleToken(tokens)
        node.add(Symbol.GREATER_THAN, tokens(state.idx))
        state.idx += 1
      }
      else {
        state.idx -= 3
        node = matchIdentifierNameExpression()
      }
    }
    node
  }


  override def parse(s: String): MyParseResult = {
    state.idx = 0
    state.indentLevel = 0
    val lexer = Tokenizer()
    tokens = lexer.lex(s).asScala.toVector
    parseResult = MyParseResult(SOURCE_TEXT)
    val list = MySyntaxNode(LIST)
    boundary:
      while (state.idx < tokens.length) {
        if (isTypeDefStart(tokens(state.idx))) {
          list.add(matchTypeDef())
        } else {
          while (state.idx < tokens.length && !isTypeDefStart(tokens(state.idx))) {
            state.idx += 1
          }
        }
      }
    if (list.slotCount() == 0) {
      parseResult.addToRoot(null)
    }
    else {
      parseResult.addToRoot(list)
    }
    parseResult
  }

  def setTokens(newTokens: Vector[Token]): Unit = {
    tokens = newTokens
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