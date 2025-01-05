package LanguageServerImplementation

import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{MemberSymbol, SymbolKind, TypeLikeSymbol, TypeSymbol}

import scala.jdk.CollectionConverters.*
import java.util
import scala.collection.mutable.ListBuffer

case class MyTypeSymbol(
                    var typeArgs: ListBuffer[TypeLikeSymbol] = ListBuffer.empty,
                    var memberSymbols: ListBuffer[MemberSymbol] = ListBuffer.empty,
                    var baseTypes: ListBuffer[TypeSymbol] = ListBuffer.empty,
                    kind: SymbolKind,
                    name: String,
                    definition: SyntaxNode,
                    var isAbstract: Boolean
                   )
  extends TypeSymbol {
  
  override def typeArguments(): util.List[_ <: TypeLikeSymbol] = typeArgs.toList.asJava

  override def originalDefinition(): TypeSymbol = ???

  override def construct(list: util.List[_ <: TypeLikeSymbol]): TypeSymbol = ???

  override def members(): util.List[_ <: MemberSymbol] = memberSymbols.toList.asJava
  
  

}
