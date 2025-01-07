package LanguageServerImplementation

import syspro.tm.parser.SyntaxNode
import syspro.tm.symbols.{FunctionSymbol, SemanticSymbol, SymbolKind, TypeLikeSymbol, VariableSymbol}

import java.util
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

case class MyFunctionSymbol(isNative: Boolean,
                            isVirtual: Boolean,
                            isAbstract: Boolean,
                            isOverride: Boolean,
                            var functionParameters: ListBuffer[VariableSymbol] = ListBuffer.empty,
                            returnType: TypeLikeSymbol,
                            var functionLocals: ListBuffer[VariableSymbol] = ListBuffer.empty,
                            owner: SemanticSymbol,
                            kind: SymbolKind,
                            name: String,
                            definition: SyntaxNode
                           ) extends FunctionSymbol {

  override def parameters(): util.List[_ <: VariableSymbol] = functionParameters.asJava

  override def locals(): util.List[_ <: VariableSymbol] = functionLocals.asJava

  override def toString: String = name

  override def hashCode(): Int = kind.hashCode() + name.hashCode + definition.hashCode()  + isVirtual.hashCode() + isOverride.hashCode() +
    isAbstract.hashCode() + isNative.hashCode()
}
