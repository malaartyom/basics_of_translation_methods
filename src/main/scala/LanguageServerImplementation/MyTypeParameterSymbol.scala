package LanguageServerImplementation

import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{SemanticSymbol, SymbolKind, TypeLikeSymbol, TypeParameterSymbol}

import java.util
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

case class MyTypeParameterSymbol(
                                  var typeParamBounds: ListBuffer[TypeLikeSymbol] = ListBuffer.empty,
                                  owner: SemanticSymbol = null,
                                  kind: SymbolKind = null,
                                  name: String,
                                  definition: SyntaxNode = null
                                )                           
  extends TypeParameterSymbol {

  override def bounds(): util.List[_ <: TypeLikeSymbol] = typeParamBounds.asJava
  override def toString: String = name
}
