package LanguageServerImplementation
import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{MemberSymbol, SymbolKind, TypeLikeSymbol, TypeSymbol}
import scala.jdk.CollectionConverters.*

import java.util

class BaseTypeSymbol(val typeName: String) extends TypeSymbol {

  override def isAbstract: Boolean = false

  override def baseTypes(): util.List[_ <: TypeSymbol] = Vector.empty.toList.asJava

  override def typeArguments(): util.List[_ <: TypeLikeSymbol] = Vector.empty.toList.asJava

  override def originalDefinition(): TypeSymbol = this

  override def construct(list: util.List[_ <: TypeLikeSymbol]): TypeSymbol = ???

  override def members(): util.List[_ <: MemberSymbol] = Vector.empty.toList.asJava

  override def kind(): SymbolKind = SymbolKind.CLASS

  override def name(): String = typeName

  override def definition(): SyntaxNode = null
}
