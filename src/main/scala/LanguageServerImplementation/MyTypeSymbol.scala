package LanguageServerImplementation

import ParserImplementation.Parsing.{MyParseResult, MySyntaxNode}
import syspro.tm.lexer.IdentifierToken
import syspro.tm.parser.{SyntaxKind, SyntaxNode}
import syspro.tm.symbols.{MemberSymbol, SymbolKind, TypeLikeSymbol, TypeSymbol}

import scala.jdk.CollectionConverters.*
import java.util
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import SyntaxNodeExtension.*

import scala.collection.mutable

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
  
  var env: TypeEnvironment = mutable.HashMap[String, TypeSymbol]()
  
  override def typeArguments(): util.List[_ <: TypeLikeSymbol] = typeArgs.asJava

  override def baseTypes(): util.List[? <: TypeSymbol] = baseTypesBuffer.asJava

  override def originalDefinition(): TypeSymbol = if (originalDef == null) this else originalDef

  override def construct(list: util.List[_ <: TypeLikeSymbol]): TypeSymbol = super.constr(list, typeArgs, definition = definition, `def` = this, types = env)

  override def members(): util.List[_ <: MemberSymbol] = memberSymbols.asJava

  override def toString: String = name

  def setEnvironment(environment: TypeEnvironment) = env = environment 

  
}
