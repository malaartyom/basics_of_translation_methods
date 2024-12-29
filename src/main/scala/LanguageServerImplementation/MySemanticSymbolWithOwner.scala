package LanguageServerImplementation

import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{SemanticSymbol, SemanticSymbolWithOwner, SymbolKind}

class MySemanticSymbolWithOwner extends SemanticSymbolWithOwner {

  override def owner(): SemanticSymbol = ???

  override def kind(): SymbolKind = ???

  override def name(): String = ???

  override def definition(): SyntaxNode = ???
}
