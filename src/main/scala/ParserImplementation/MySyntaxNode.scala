package ParserImplementation
import syspro.tm.lexer.Token
import syspro.tm.parser.{AnySyntaxKind, SyntaxNode}

import scala.collection.mutable.ListBuffer


class MySyntaxNode(var1: AnySyntaxKind = null, var2: Token = null) extends SyntaxNode {
  var tokenKind: AnySyntaxKind = var1
  var tokenType: Token = var2
  var children: ListBuffer[MySyntaxNode] = ListBuffer[MySyntaxNode]()

  override def kind(): AnySyntaxKind = tokenKind

  override def token(): Token = tokenType

  override def slotCount(): Int = children.length

  override def slot(i: Int): SyntaxNode = children(i)
  
  def apply(i: Int): MySyntaxNode = children(i)
  
  def add(kind: AnySyntaxKind, token: Token = null): ListBuffer[MySyntaxNode] = 
    children.append(MySyntaxNode(kind, token))
  
  def add(mySyntaxNode: MySyntaxNode): ListBuffer[MySyntaxNode] = children.append(mySyntaxNode)

  def addNull(num: Int) = for (x <- 1 to num) {children.append(null)}
  
  def addLeft(mySyntaxNode: MySyntaxNode): Unit = children.insert(0, mySyntaxNode)

  override def toString: String = tokenKind.toString


}
