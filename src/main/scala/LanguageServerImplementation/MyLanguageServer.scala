package LanguageServerImplementation

import syspro.tm.symbols.{LanguageServer, MemberSymbol, SemanticModel, SymbolKind, TypeParameterSymbol, TypeSymbol}
import syspro.tm.parser.SyntaxKind.*
import syspro.tm.parser.SyntaxNode
import syspro.tm.lexer.Symbol
import syspro.tm.lexer.Symbol.*
import ParserImplementation.Parsing.{MyParseResult, MyParser, MySyntaxNode}
import syspro.tm.lexer.Keyword.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class MyLanguageServer extends LanguageServer {
//  var types: mutable.HashMap[String, TypeSymbol] = mutable.HashMap(
//    "Boolean" -> BaseTypeSymbol("Boolean"),
//    "Int32" -> BaseTypeSymbol("Int32"),
//    "Int64" -> BaseTypeSymbol("Int64"),
//    "UInt32" -> BaseTypeSymbol("UInt32"),
//    "UInt64" -> BaseTypeSymbol("UInt64"),
//    "Rune" -> BaseTypeSymbol("Rune")
//  )
//
//  def checkTypeParamDef(prevNode: SyntaxNode): MySyntaxNodeWithSymbol = ???
//
//  def typeParameterSymbols(prevNode: MySyntaxNode): TypeParameterSymbol =
//    val node = MyTypeParameterSymbol(
//      typeParamName = ???,
//      typeParamKind = ???,
//      typeParamOwner = ???,
//      typeParamBounds = ???,
//      typeParamDefinition = ???
//    )
//    node
//
//  def memberSymbol(prevNode: MySyntaxNode): MemberSymbol = ???
//
//  def checkVariableDefinition(node: MySyntaxNode) = ???
//
//  def checkFuntionDefinition(node: MySyntaxNode) = ???
//
//  def checkDefinition(prevNode: SyntaxNode): MySyntaxNodeWithSymbol =
//    prevNode.kind() match
//      case TYPE_DEFINITION => checkTypeDefinition(prevNode.asInstanceOf[MySyntaxNode])
//      case VARIABLE_DEFINITION => checkVariableDefinition(prevNode.asInstanceOf[MySyntaxNode])
//      case FUNCTION_DEFINITION => checkFuntionDefinition(prevNode.asInstanceOf[MySyntaxNode])
//      case PARAMETER_DEFINITION => ???
//      case TYPE_PARAMETER_DEFINITION => checkTypeParamDef(prevNode)
//
//  def checkTypeDefChildren(prevNode: SyntaxNode): MySyntaxNodeWithSymbol =
//    val node = MySyntaxNodeWithSymbol(prevNode.asInstanceOf[MySyntaxNode])
//    prevNode.kind() match
//      case SEPARATED_LIST => node.children.map(x => checkTypeParamDef(x));
//      case LIST => node.children.map(x => checkDefinition(x))
//    node
//
//
//  def checkTypeDefinition(node: SyntaxNode): MySyntaxNodeWithSymbol =
//    val symbol = MyTypeSymbol(
//      typeArgs = 
//        (for {
//        _ <- prevNode.children.find(x => x.kind() == LESS_THAN)
//        v <- prevNode.children.find(x => x.kind() == SEPARATED_LIST)
//        _ <- prevNode.children.find(x => x.kind() == GREATER_THAN)
//      } yield v.children.map(param => typeParameterSymbols(param))).get,
//
//      memberSymbols = (for {
//        d <- prevNode.children.find(x => x.kind() == LIST)
//      } yield d.children.map(member => memberSymbol(member))).get,
//
//      parentTypes = ListBuffer[TypeSymbol](),
//      typeKind = prevNode.slot(0).kind() match
//        case CLASS => SymbolKind.CLASS
//        case OBJECT => SymbolKind.OBJECT
//        case INTERFACE => SymbolKind.INTERFACE,  
//      typeName = prevNode.slot(1).token().toString,
//      typeDef = prevNode , // TODO: Check if it is definition or not
//      abstractFlag = false  // TODO 
//    )
//    val symbol = MyTypeSymbol(
//      typeArgs = ???,
//      memberSymbols = ???,
//      parentTypes = ???,
//      typeKind = ???,
//      typeName = ???,
//      typeDef = ???,
//      abstractFlag = ???
//    )
//
//
//    val node = MySyntaxNodeWithSymbol(prevNode, symbol)
//    node.children.map(x => checkTypeDefChildren(x))
//    node


  override def buildModel(s: String): SemanticModel =
    MySemanticModel(MyParser().parse(s))
