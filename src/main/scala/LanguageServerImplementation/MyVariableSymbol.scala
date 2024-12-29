package LanguageServerImplementation

import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{SemanticSymbol, SymbolKind, TypeLikeSymbol, VariableSymbol}

class MyVariableSymbol extends VariableSymbol {

  override def `type`(): TypeLikeSymbol = ???

  override def owner(): SemanticSymbol = ???

  override def kind(): SymbolKind = ???

  override def name(): String = ???

  override def definition(): SyntaxNode = ???
}
