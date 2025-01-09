package LanguageServerImplementation

import syspro.tm.lexer.Keyword
import syspro.tm.parser.{SyntaxKind, SyntaxNode}
import syspro.tm.symbols.{SemanticSymbol, SymbolKind}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object SyntaxNodeExtension {
  val OPTION_NAME_EXPRESSION_NAME = 1
  private val TYPE_DEF_PARENTS = 5
  private val TYPE_BOUND_LIST = 1
  private val TYPE_DEFINITION_TERMINAL = 0
  private val TYPE_DEF_IDENTIFIER = 1
  private val ID_NAME_EXPR_IDENTIFIER = 0
  private val OP_NAME_EXPR_NAME_EXPR = 1


  extension (node: SyntaxNode) {
    def children: ListBuffer[SyntaxNode] =
      if (node == null) return ListBuffer.empty
      ListBuffer[SyntaxNode]((0 until node.slotCount()).map(node.slot)*)

    def typeArgs: ListBuffer[SyntaxNode] =
      node.kind() match
        case SyntaxKind.TYPE_DEFINITION => node.slot(3).children.filter(x => x.kind() == SyntaxKind.TYPE_PARAMETER_DEFINITION)

    def symbolKind: SymbolKind = node.kind() match
      case SyntaxKind.TYPE_DEFINITION => node.slot(0).kind() match // TODO: Avoid magic const
        case Keyword.CLASS =>  SymbolKind.CLASS
        case Keyword.INTERFACE => SymbolKind.INTERFACE
        case Keyword.OBJECT => SymbolKind.OBJECT
      case SyntaxKind.FUNCTION_DEFINITION => SymbolKind.FUNCTION
      case SyntaxKind.PARAMETER_DEFINITION => SymbolKind.PARAMETER
      case SyntaxKind.VARIABLE_DEFINITION_STATEMENT => SymbolKind.LOCAL
      case SyntaxKind.IDENTIFIER_NAME_EXPRESSION => SymbolKind.LOCAL
      case SyntaxKind.TYPE_PARAMETER_DEFINITION => SymbolKind.TYPE_PARAMETER
      case SyntaxKind.IDENTIFIER_NAME_EXPRESSION | SyntaxKind.OPTION_NAME_EXPRESSION | SyntaxKind.GENERIC_NAME_EXPRESSION => SymbolKind.CLASS
      case SyntaxKind.IDENTIFIER => SymbolKind.LOCAL
      case _ => null // TODO


    @tailrec
    def name: String =
      node.kind() match
        case SyntaxKind.TYPE_DEFINITION => node.slot(TYPE_DEF_IDENTIFIER).name
        case SyntaxKind.IDENTIFIER => node.token().toString
        case Keyword.THIS => node.token().toString
        case SyntaxKind.IDENTIFIER_NAME_EXPRESSION => node.slot(ID_NAME_EXPR_IDENTIFIER).name // TODO: AVOID MAGIC CONSTANTS
        case SyntaxKind.OPTION_NAME_EXPRESSION => node.slot(OP_NAME_EXPR_NAME_EXPR).name
        case SyntaxKind.GENERIC_NAME_EXPRESSION => node.slot(0).name
        case SyntaxKind.FUNCTION_DEFINITION => node.slot(2).name
        case SyntaxKind.PARAMETER_DEFINITION => node.slot(0).name
        case SyntaxKind.VARIABLE_DEFINITION => node.slot(1).name
        case SyntaxKind.VARIABLE_DEFINITION_STATEMENT => node.slot(0).name
        case SyntaxKind.TYPE_PARAMETER_DEFINITION => node.slot(0).name
        // TODO: Other cases

    def parents: ListBuffer[SyntaxNode] =
      node.kind() match
        case SyntaxKind.TYPE_DEFINITION =>
          if (node.slot(5) != null)
            return node.slot(5).slot(1).children.filter(isNameExpr)
          ListBuffer.empty
        case SyntaxKind.TYPE_PARAMETER_DEFINITION =>
          if (node.slot(1) != null)
            return node.slot(1).slot(1).children.filter(isNameExpr) // TODO: slot(1).children.filter(isNameExpr) -> typeBounds
          ListBuffer.empty
        case SyntaxKind.IDENTIFIER_NAME_EXPRESSION => ???
          
      
    def definitions: ListBuffer[SyntaxNode] =
      node.kind() match
        case SyntaxKind.TYPE_DEFINITION => node.slot(7).children // TODO: Avoid magic const
        case _ => ListBuffer.empty

    def returnType: SyntaxNode =
      node.kind() match
        case SyntaxKind.FUNCTION_DEFINITION => node.slot(7) // TODO:
        case _ =>  throw RuntimeException("Cannot get returnType at non-function node")

    def `type`: SyntaxNode =
      node.kind() match
        case SyntaxKind.PARAMETER_DEFINITION => node.slot(2)
        case SyntaxKind.VARIABLE_DEFINITION => node.slot(3)
        case SyntaxKind.VARIABLE_DEFINITION_STATEMENT => node.slot(0).`type`
        case SyntaxKind.IDENTIFIER_NAME_EXPRESSION => null

    def expression: SyntaxNode =
      node.kind() match
        case SyntaxKind.EXPRESSION_STATEMENT => node.slot(0)
        case _ => throw RuntimeException("Not an expression statement")

    def forLocal: SyntaxNode =
      node.kind() match
        case SyntaxKind.FOR_STATEMENT => node.slot(1)
        case _ => throw RuntimeException("Not FOR_STATEMENT IN forLocal")

    def forStatements: ListBuffer[SyntaxNode] =
      node.kind() match
        case SyntaxKind.FOR_STATEMENT => node.slot(5).children
        case _ => throw RuntimeException("Not FOR_STATEMENT IN forStatements")
        
    def genericParams: ListBuffer[SyntaxNode] =
      node.kind() match
        case SyntaxKind.GENERIC_NAME_EXPRESSION => node.slot(2).children
        case _ => throw RuntimeException("Not a GENERIC_NAME_EXPRESSION")

    def functionParameters: ListBuffer[SyntaxNode] =
      node.kind() match
        case SyntaxKind.FUNCTION_DEFINITION => node.slot(4).children.filter(isParameterDefinition) // TODO: Avoid magic const

    def functionStatements: ListBuffer[SyntaxNode] =
      node.kind() match
        case SyntaxKind.FUNCTION_DEFINITION => node.slot(9).children
        case _ => throw RuntimeException("Not a function!")

    def isVirtual(owner: SemanticSymbol): Boolean =
      node.kind() match
        case SyntaxKind.FUNCTION_DEFINITION =>
          node.slot(0).children.exists(x => x.kind() == Keyword.VIRTUAL) || node.isOverride || node.isAbstract(owner)
        case _ => throw RuntimeException("Not a Function in function isVirtual")

    def isNative: Boolean =
      node.kind() match
        case SyntaxKind.FUNCTION_DEFINITION => node.slot(0).children.exists(x => x.kind() == Keyword.NATIVE)
//          .map(x => x.kind()).contains(Keyword.NATIVE)
        case _ => throw RuntimeException("Not a Function in function isNative")

    def isAbstract(owner: SemanticSymbol): Boolean =
      node.kind() match
        case SyntaxKind.FUNCTION_DEFINITION => node.slot(0).children.exists(x => x.kind() == Keyword.ABSTRACT)
//          map(x => x.kind()).contains(Keyword.ABSTRACT)
          || owner.kind() == SymbolKind.INTERFACE
        case _ => throw RuntimeException("Not a Function in function isAbstract")

    def isOverride: Boolean =
      node.kind() match
        case SyntaxKind.FUNCTION_DEFINITION => node.slot(0).children.exists(x => x.kind() == Keyword.OVERRIDE)
//          map(x => x.kind()).contains(Keyword.OVERRIDE)
        case _ => throw RuntimeException("Not a Function in function isOverride")
        
        
  }


  def isNameExpr(node: SyntaxNode): Boolean =
    node.kind() match
      case SyntaxKind.IDENTIFIER_NAME_EXPRESSION | SyntaxKind.OPTION_NAME_EXPRESSION | SyntaxKind.GENERIC_NAME_EXPRESSION => true
      case _ => false

  private def isParameterDefinition(node: SyntaxNode): Boolean =
    node.kind() match
      case SyntaxKind.PARAMETER_DEFINITION => true
      case _ => false
  

}
