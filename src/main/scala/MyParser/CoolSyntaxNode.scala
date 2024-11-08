package MyParser
import syspro.tm.lexer.Token
import syspro.tm.parser.{AnySyntaxKind, SyntaxNode}

import scala.collection.mutable.ListBuffer

class CoolSyntaxNode(var1: AnySyntaxKind, var2: Token) extends SyntaxNode {
  var tokenKind: AnySyntaxKind = var1
  var tokenType: Token = var2
  var children: ListBuffer[SyntaxNode] = ListBuffer[SyntaxNode]()

  override def kind(): AnySyntaxKind = tokenKind

  override def token(): Token = tokenType

  override def slotCount(): Int = children.length

  override def slot(i: Int): SyntaxNode = children(i)
  
  


}
