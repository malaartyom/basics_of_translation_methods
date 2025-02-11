package ParserImplementation.Parsing

import syspro.tm.lexer.Token
import syspro.tm.parser.{AnySyntaxKind, SyntaxNode}
import syspro.tm.symbols.{SemanticSymbol, SyntaxNodeWithSymbols}

import scala.collection.mutable.ListBuffer


class MySyntaxNode(
                    var1: AnySyntaxKind,
                    var2: Token = null,
                    var sym: SemanticSymbol = null,
                    var children: ListBuffer[SyntaxNode] = ListBuffer[SyntaxNode]()) extends SyntaxNodeWithSymbols {
  var tokenKind: AnySyntaxKind = var1
  var tokenType: Token = var2

  override def kind(): AnySyntaxKind = tokenKind

  override def token(): Token = tokenType

  override def slotCount(): Int = children.length

  override def slot(i: Int): SyntaxNode = children(i)
  
  def apply(i: Int): SyntaxNode = children(i)
  
  def add(kind: AnySyntaxKind, token: Token = null): ListBuffer[SyntaxNode] =
    children.append(MySyntaxNode(kind, token))
  
  def add(mySyntaxNode: SyntaxNode): ListBuffer[SyntaxNode] = children.append(mySyntaxNode)

  def addFail(num: Int): Unit = for (x <- 1 to num) {children.append(null)}
  
  def addLeft(mySyntaxNode: SyntaxNode): Unit = children.insert(0, mySyntaxNode)

  override def toString: String = tokenKind.toString

  override def symbol(): SemanticSymbol = sym


}
