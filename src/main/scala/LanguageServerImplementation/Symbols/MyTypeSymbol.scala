package LanguageServerImplementation.Symbols

import LanguageServerImplementation.Context.TypeEnvironment
import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{MemberSymbol, SymbolKind, TypeLikeSymbol, TypeSymbol}

import java.util
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

case class MyTypeSymbol(
                    var typeArgs: ListBuffer[TypeLikeSymbol] = ListBuffer.empty,
                    var memberSymbols: ListBuffer[MemberSymbol] = ListBuffer.empty,
                    var baseTypesBuffer: ListBuffer[TypeSymbol] = ListBuffer.empty,
                    kind: SymbolKind,
                    name: String,
                    definition: SyntaxNode,
                    var isAbstract: Boolean = false,
                    originalDef: TypeSymbol = null
                   )
  extends TypeSymbol with Constructable {

  private var env: TypeEnvironment = mutable.HashMap[String, TypeSymbol]()
  
  override def typeArguments(): util.List[? <: TypeLikeSymbol] = typeArgs.asJava

  override def baseTypes(): util.List[? <: TypeSymbol] = baseTypesBuffer.asJava

  override def originalDefinition(): TypeSymbol = if (originalDef == null) this else originalDef

  override def construct(list: util.List[? <: TypeLikeSymbol]): TypeSymbol = super.construct(list, typeArgs, definition = definition, `def` = this, types = env)

  override def members(): util.List[? <: MemberSymbol] = memberSymbols.asJava

  override def toString: String = name

  def setEnvironment(environment: TypeEnvironment): Unit = env = environment

  
}
