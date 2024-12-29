package LanguageServerImplementation

import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{FunctionSymbol, SemanticSymbol, SymbolKind, TypeLikeSymbol, VariableSymbol}

import java.util

class MyFunctionSymbol extends FunctionSymbol {
  override def isNative: Boolean = ???

  override def isVirtual: Boolean = ???

  override def isAbstract: Boolean = ???

  override def isOverride: Boolean = ???

  override def parameters(): util.List[_ <: VariableSymbol] = ???

  override def returnType(): TypeLikeSymbol = ???

  override def locals(): util.List[_ <: VariableSymbol] = ???

  override def owner(): SemanticSymbol = ???

  override def kind(): SymbolKind = ???

  override def name(): String = ???

  override def definition(): SyntaxNode = ???
}
