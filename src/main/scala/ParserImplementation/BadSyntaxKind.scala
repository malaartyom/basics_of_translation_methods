package ParserImplementation

import syspro.tm.parser.AnySyntaxKind

object BadSyntaxKind extends AnySyntaxKind {
  
  override def isTerminal = false

}
