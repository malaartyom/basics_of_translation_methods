package LanguageServerImplementation

import ParserImplementation.Parsing.MySyntaxNode
import syspro.tm.lexer.Token
import syspro.tm.parser.{AnySyntaxKind, SyntaxNode}
import syspro.tm.symbols.{SemanticSymbol, SyntaxNodeWithSymbols}

import scala.collection.mutable.ListBuffer

class MySyntaxNodeWithSymbol(
                              var node: MySyntaxNode,
                              var nodeSymbol: SemanticSymbol = null
                            ) 
  extends SyntaxNodeWithSymbols {
  
  var children: ListBuffer[SyntaxNode] = node.children
  
  override def kind(): AnySyntaxKind = node.kind()

  override def slotCount(): Int = node.slotCount()

  override def slot(i: Int): SyntaxNode = node.slot(i)

  override def token(): Token = node.token()

  override def symbol(): SemanticSymbol = nodeSymbol
  
  def add(mySyntaxNodeWithSymbol: MySyntaxNodeWithSymbol) = children.append(mySyntaxNodeWithSymbol) 
    
}
