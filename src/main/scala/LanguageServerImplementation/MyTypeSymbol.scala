package LanguageServerImplementation

import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{MemberSymbol, SymbolKind, TypeLikeSymbol, TypeSymbol}

import java.util

class MyTypeSymbol extends TypeSymbol {

  override def isAbstract: Boolean = ???

  override def baseTypes(): util.List[_ <: TypeSymbol] = ???

  override def typeArguments(): util.List[_ <: TypeLikeSymbol] = ???

  override def originalDefinition(): TypeSymbol = ???

  override def construct(list: util.List[_ <: TypeLikeSymbol]): TypeSymbol = ???

  override def members(): util.List[_ <: MemberSymbol] = ???

  override def kind(): SymbolKind = ???

  override def name(): String = ???

  override def definition(): SyntaxNode = ???
}
