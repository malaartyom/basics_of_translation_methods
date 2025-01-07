package LanguageServerImplementation

import ParserImplementation.Parsing.MySyntaxNode
import syspro.tm.lexer.Token
import syspro.tm.parser.{AnySyntaxKind, SyntaxNode}
import syspro.tm.symbols.{SemanticSymbol, SyntaxNodeWithSymbols}

import scala.collection.mutable.ListBuffer

import scala.jdk.CollectionConverters.*

class MySyntaxNodeWithSymbol(
                              var node: SyntaxNode,
                              var nodeSymbol: SemanticSymbol = null,
                              var children: ListBuffer[MySyntaxNodeWithSymbol]
                            )
  extends SyntaxNodeWithSymbols {

//  private def convert(node: SyntaxNode): MySyntaxNodeWithSymbol = {
//    node match
//      case _: MySyntaxNode => MySyntaxNodeWithSymbol(node)
//      case x: MySyntaxNodeWithSymbol => x
//      case _ if node == null => null
//  }
//
//  var children: ListBuffer[MySyntaxNodeWithSymbol] = ListBuffer(node.descendants(false).asScala.toSeq*).map(convert)

  override def kind(): AnySyntaxKind = node.kind()

  override def slotCount(): Int = node.slotCount()

  override def slot(i: Int): SyntaxNode = children(i)

  override def token(): Token = node.token()

  override def symbol(): SemanticSymbol = nodeSymbol

}
