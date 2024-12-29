package LanguageServerImplementation

import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{SemanticSymbol, SymbolKind, TypeLikeSymbol, TypeParameterSymbol}

import java.util

class MyTypeParameterSymbol extends TypeParameterSymbol {

  override def bounds(): util.List[_ <: TypeLikeSymbol] = ???

  override def owner(): SemanticSymbol = ???

  override def kind(): SymbolKind = ???

  override def name(): String = ???

  override def definition(): SyntaxNode = ???
}
