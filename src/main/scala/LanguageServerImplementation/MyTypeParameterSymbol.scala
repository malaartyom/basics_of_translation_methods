package LanguageServerImplementation

import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{SemanticSymbol, SymbolKind, TypeLikeSymbol, TypeParameterSymbol}

import java.util
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

class MyTypeParameterSymbol(
                             val typeParamName: String,
                             val typeParamKind: SymbolKind,
                             val typeParamOwner: SemanticSymbol,
                             val typeParamBounds: ListBuffer[TypeLikeSymbol],
                             val typeParamDefinition: SyntaxNode
                           )                           
  extends TypeParameterSymbol {

  override def bounds(): util.List[_ <: TypeLikeSymbol] = typeParamBounds.asJava

  override def owner(): SemanticSymbol = typeParamOwner

  override def kind(): SymbolKind = typeParamKind

  override def name(): String = typeParamName

  override def definition(): SyntaxNode = typeParamDefinition
}
