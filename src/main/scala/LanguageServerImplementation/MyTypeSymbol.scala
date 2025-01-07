package LanguageServerImplementation

import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{MemberSymbol, SymbolKind, TypeLikeSymbol, TypeSymbol}

import scala.jdk.CollectionConverters.*
import java.util
import scala.collection.mutable.ListBuffer

case class MyTypeSymbol(
                    var typeArgs: ListBuffer[TypeLikeSymbol] = ListBuffer.empty,
                    var memberSymbols: ListBuffer[MemberSymbol] = ListBuffer.empty,
                    var baseTypesBuffer: ListBuffer[TypeSymbol] = ListBuffer.empty,
                    kind: SymbolKind,
                    name: String,
                    definition: SyntaxNode,
                    var isAbstract: Boolean = false 
                   )
  extends TypeSymbol {
  
  override def typeArguments(): util.List[_ <: TypeLikeSymbol] = typeArgs.asJava

  override def baseTypes(): util.List[? <: TypeSymbol] = baseTypesBuffer.asJava

  override def originalDefinition(): TypeSymbol = this

  override def construct(list: util.List[_ <: TypeLikeSymbol]): TypeSymbol = ???

  override def members(): util.List[_ <: MemberSymbol] = memberSymbols.asJava

  override def toString: String = name
  
  

}
