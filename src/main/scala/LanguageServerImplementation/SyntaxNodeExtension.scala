package LanguageServerImplementation

import syspro.tm.lexer.Keyword
import syspro.tm.parser.{SyntaxKind, SyntaxNode}
import syspro.tm.symbols.SymbolKind


import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object SyntaxNodeExtension {
  val OPTION_NAME_EXPRESSION_NAME = 1
  val TYPE_DEF_PARENTS = 5
  val TYPE_BOUND_LIST = 1

  extension (node: SyntaxNode) {
    def children: ListBuffer[SyntaxNode] =
      ListBuffer[SyntaxNode]((0 until node.slotCount()).map(node.slot)*)
    def symbolKind: SymbolKind = node.kind() match
      case Keyword.CLASS =>  SymbolKind.CLASS
      case Keyword.INTERFACE => SymbolKind.INTERFACE
      case Keyword.OBJECT => SymbolKind.OBJECT
      case _ => null // TODO


    @tailrec
    def name: String =
      node.kind() match
        case SyntaxKind.TYPE_DEFINITION => node.slot(1).name
        case SyntaxKind.IDENTIFIER => node.token().toString
        case SyntaxKind.IDENTIFIER_NAME_EXPRESSION => node.slot(0).name // TODO: AVOID MAGIC CONSTANTS
        case SyntaxKind.OPTION_NAME_EXPRESSION => node.slot(1).name
        case SyntaxKind.GENERIC_NAME_EXPRESSION => node.slot(0).name
        // TODO: Other cases

    def parents: ListBuffer[SyntaxNode] =             // TODO: Rename parents -> typedefParents | switch-case
      if (node.slot(TYPE_DEF_PARENTS) != null)
        node.slot(TYPE_DEF_PARENTS).slot(TYPE_BOUND_LIST).children.filter(isNameExpr)
      ListBuffer.empty
      
    def definitions: ListBuffer[SyntaxNode] =
      node.kind() match
        case SyntaxKind.TYPE_DEFINITION => node.slot(7).children // TODO: Avoid magic const
        case _ => ListBuffer.empty

  }


  private def isNameExpr(node: SyntaxNode): Boolean =
    node.kind() match
      case SyntaxKind.IDENTIFIER_NAME_EXPRESSION | SyntaxKind.OPTION_NAME_EXPRESSION | SyntaxKind.GENERIC_NAME_EXPRESSION => true
      case _ => false
      
      
}
