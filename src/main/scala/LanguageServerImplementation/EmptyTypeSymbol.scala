package LanguageServerImplementation

import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{MemberSymbol, SymbolKind, TypeLikeSymbol, TypeSymbol}

import java.util
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

case class EmptyTypeSymbol(name: String) extends TypeSymbol with Constructable {

  override def isAbstract: Boolean = false

  override def baseTypes(): util.List[_ <: TypeSymbol] = Vector.empty.toList.asJava

  override def typeArguments(): util.List[_ <: TypeLikeSymbol] = Vector.empty.toList.asJava

  override def originalDefinition(): TypeSymbol = this

  override def construct(list: util.List[_ <: TypeLikeSymbol]): TypeSymbol = super.constr(list, ListBuffer.empty, null, this)

  override def members(): util.List[_ <: MemberSymbol] = Vector.empty.toList.asJava

  override def kind(): SymbolKind = null

  override def definition(): SyntaxNode = null

  override def toString: String = "empty" + name

}
