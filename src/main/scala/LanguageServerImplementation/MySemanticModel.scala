package LanguageServerImplementation

import ParserImplementation.Parsing.{MyParseResult, MySyntaxNode}
import syspro.tm.lexer.Keyword
import syspro.tm.parser.{Diagnostic, SyntaxKind, SyntaxNode, TextSpan}
import syspro.tm.symbols.{MemberSymbol, SemanticModel, SymbolKind, TypeSymbol}

import scala.jdk.CollectionConverters.*
import java.util
import scala.collection.mutable.ListBuffer
import SyntaxNodeExtension.*

import scala.collection.mutable


class MySemanticModel(var parseResult: MyParseResult)
  extends SemanticModel {
  var ranges: ListBuffer[TextSpan] = parseResult.invalid_ranges
  var diagnostic: ListBuffer[Diagnostic] = parseResult.diagnostic
  var rootNode: MySyntaxNode = parseResult.rootNode
  var types: mutable.HashMap[String, TypeSymbol] = mutable.HashMap(
    "Boolean" -> BaseTypeSymbol("Boolean"),
    "Int32" -> BaseTypeSymbol("Int32"),
    "Int64" -> BaseTypeSymbol("Int64"),
    "UInt32" -> BaseTypeSymbol("UInt32"),
    "UInt64" -> BaseTypeSymbol("UInt64"),
    "Rune" -> BaseTypeSymbol("Rune")
  )


  override def root(): SyntaxNode = rootNode

  override def invalidRanges(): util.Collection[TextSpan] = parseResult.invalidRanges()

  override def diagnostics(): util.Collection[Diagnostic] = parseResult.diagnostics()

  override def lookupType(s: String): TypeSymbol = ???

  override def typeDefinitions(): util.List[_ <: TypeSymbol] =
    val list = ListBuffer[MyTypeSymbol]()
    for (i <- 0 to this.root().slotCount()) {
      list.append(checkTypeDefenition(rootNode.slot(i)))
    }
    list.asJava

  def checkTypeDefenition(node: SyntaxNode): MyTypeSymbol =
    types += (node.name -> null)
    val symbol = MyTypeSymbol(
      kind = node.symbolKind,
      name = node.name,
      definition = node,
      isAbstract = false)
    symbol.typeArgs = ???
    symbol.baseTypes = getParents(node)
    symbol.memberSymbols = getMembers(node)
    symbol.isAbstract = checkAbstractness(node)
    types(symbol.name) = symbol
    symbol

  def getParents(node: SyntaxNode): ListBuffer[TypeSymbol] = node.parents.map(getNameExpression)

  def getNameExpression(node: SyntaxNode): TypeSymbol =
    // TODO: Maybe problem if func called not from get parents
    // <: SomeClass & Parent1 & ?Parent2 & List<Int> & Parent3<Some>
    node.kind() match
      case SyntaxKind.IDENTIFIER_NAME_EXPRESSION => types(node.name)
      case SyntaxKind.GENERIC_NAME_EXPRESSION => types(node.name);
      case SyntaxKind.OPTION_NAME_EXPRESSION => getNameExpression(node.slot(OPTION_NAME_EXPRESSION_NAME)) 
      case _ => throw RuntimeException(s"Not a name expression in getNameExpression ${node}") // TODO: Error Maybe throw exception

  def getMembers(node: SyntaxNode): ListBuffer[MemberSymbol] = node.definitions.map(x => getDefinition(x, node))
  
  def getDefinition(node: SyntaxNode, owner: SyntaxNode): MemberSymbol =
    node.kind() match
      case SyntaxKind.FUNCTION_DEFINITION =>
        MyFunctionSymbol(
          isNative = ???,
          isVirtual = ???,
          isAbstract = ???,
          isOverride = ???,
          functionParameters = ???,
          returnType = ???,
          functionLocals = ???,
          owner = ???,
          kind = ???,
          name = ???,
          definition = ???)
      case SyntaxKind.VARIABLE_DEFINITION =>
        MyVariableSymbol()
      case _ => throw RuntimeException("Not a variable or a function!")


  def checkAbstractness(node: SyntaxNode): Boolean = false

//class Parent3<T <: Int>
//  var x: T
//
//  def bro(x: T): T
//    println("bro")
//    return x
//
//
//class X <: Parent3<Int> & Parent2
//  def pass()
//    return 4