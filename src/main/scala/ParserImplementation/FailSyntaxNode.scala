package ParserImplementation

import syspro.tm.lexer.Token
import syspro.tm.parser.{AnySyntaxKind, SyntaxNode}

class FailSyntaxNode extends SyntaxNode {

  override def kind(): AnySyntaxKind = null

  override def token(): Token = null

  override def slotCount(): Int = 0

  override def slot(i: Int): SyntaxNode = FailSyntaxNode()

}
