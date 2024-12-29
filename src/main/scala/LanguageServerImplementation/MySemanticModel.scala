package LanguageServerImplementation

import ParserImplementation.MySyntaxNode
import syspro.tm.parser.{Diagnostic, SyntaxNode, TextSpan}
import syspro.tm.symbols.{SemanticModel, TypeSymbol}
import scala.jdk.CollectionConverters.*


import java.util

class MySemanticModel(var rootNode: SyntaxNode = MySyntaxNode(null, null),
                      var ranges: Vector[TextSpan] = Vector.empty,
                      var diagnostic: Vector[Diagnostic] = Vector.empty
                     )
  extends SemanticModel {
  override def root(): SyntaxNode = rootNode

  override def invalidRanges(): util.Collection[TextSpan] = ranges.asJavaCollection

  override def diagnostics(): util.Collection[Diagnostic] = diagnostic.asJavaCollection

  override def lookupType(s: String): TypeSymbol = ???

  override def typeDefinitions(): util.List[_ <: TypeSymbol] = ???
  
}

