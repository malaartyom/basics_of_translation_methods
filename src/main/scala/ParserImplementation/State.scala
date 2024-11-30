package ParserImplementation

import ParserImplementation.Checkers.isDedent
import syspro.tm.lexer.{Symbol, SymbolToken, Token}
import syspro.tm.parser.AnySyntaxKind
import syspro.tm.lexer.Symbol.*

class State {
  var idx = 0
  var length = 0
  private var openLessThan = false
  private var openParen = false
  private var openBracket = false

  var indentLevel = 0
  
  var tokens: Vector[Token] = Vector[Token]()

  def doubleToken(tokens: Vector[Token]): Vector[Token] = {
    tokens(idx) match
      case greater: SymbolToken if greater.symbol == GREATER_THAN_GREATER_THAN =>
        val firstToken = SymbolToken(greater.start, greater.end - greater.trailingTriviaLength - 1, greater.leadingTriviaLength, 0, GREATER_THAN)
        val secondToken = SymbolToken(greater.end - greater.trailingTriviaLength, greater.end, 0, greater.trailingTriviaLength, GREATER_THAN)
        tokens.patch(idx, Vector[Token](firstToken, secondToken), 1)
  }

       
  
}
