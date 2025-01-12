package LanguageServerImplementation.Symbols

import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{SemanticSymbol, SymbolKind, TypeLikeSymbol, VariableSymbol}

case class MyVariableSymbol(`type`: TypeLikeSymbol,
                             owner: SemanticSymbol,
                             kind: SymbolKind,
                             name: String,
                             definition: SyntaxNode 
                           ) extends VariableSymbol {
  override def toString: String = name


  override def hashCode(): Int = name.hashCode + kind.hashCode()
  
  
  
}