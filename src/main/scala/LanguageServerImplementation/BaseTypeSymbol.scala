package LanguageServerImplementation
import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{MemberSymbol, SymbolKind, TypeLikeSymbol, TypeSymbol}

import scala.jdk.CollectionConverters.*
import java.util
import scala.collection.mutable.ListBuffer

class BaseTypeSymbol(
                      val typeName: String,
                      val parents: ListBuffer[TypeSymbol] = ListBuffer.empty,
                      val generics: Seq[String] = Seq.empty,
                      val symbolKind: SymbolKind = SymbolKind.CLASS
                    ) extends TypeSymbol {

  override def isAbstract: Boolean = false

  override def baseTypes(): util.List[_ <: TypeSymbol] = parents.asJava

  override def typeArguments(): util.List[_ <: TypeLikeSymbol] =
    if (generics.isEmpty) return Vector.empty.toList.asJava
    generics.map(s => {
      MyTypeParameterSymbol(
        typeParamBounds = ListBuffer.empty,
        owner = this,
        kind = SymbolKind.TYPE_PARAMETER,
        name = s,
        definition = null)
    }).asJava

  override def originalDefinition(): TypeSymbol = this

  override def construct(list: util.List[_ <: TypeLikeSymbol]): TypeSymbol = ???

  override def members(): util.List[_ <: MemberSymbol] = Vector.empty.toList.asJava

  override def kind(): SymbolKind = symbolKind

  override def name(): String = typeName

  override def definition(): SyntaxNode = null

  override def toString: String = typeName
}
