package ParserImplementation

import syspro.tm.lexer.Token
import syspro.tm.parser.AnySyntaxKind
import State.OpenOrClose.{PAREN, LESS_THAN, BRACKET} 
import State.OpenOrClose

class State {
  var idx = 0
  var length = 0
  private var openLessThan = false
  private var openParen = false
  private var openBracket = false 
  
  var buffOfNodes: Vector[MySyntaxNode] = Vector[MySyntaxNode]()
  
  def drop(): Unit = length = 0
  
  def open(openType: OpenOrClose): Unit = {
    openType match
      case LESS_THAN => openLessThan = true
      case PAREN => openParen = true
      case BRACKET => openBracket = true
  }
  
  def isOpen(openType: OpenOrClose): Boolean = {
    openType match
      case LESS_THAN => openLessThan
      case PAREN => openParen
      case BRACKET => openBracket
  }
  
}


object State {
  enum OpenOrClose {
    case LESS_THAN
    case PAREN
    case BRACKET
  }
}
