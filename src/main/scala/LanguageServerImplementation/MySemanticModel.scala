package LanguageServerImplementation

import LanguageServerImplementation.SyntaxNodeExtension.*
import ParserImplementation.Parsing.MyParseResult
import syspro.tm.parser.{Diagnostic, SyntaxKind, SyntaxNode, TextSpan}
import syspro.tm.symbols.*
import syspro.tm.{lexer, parser}

import java.util
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*


class MySemanticModel(var parseResult: MyParseResult, var rootNode: SyntaxNode)
  extends SemanticModel {

  var code: Int = 0
  var ranges: ListBuffer[TextSpan] = parseResult.invalid_ranges
  var diagnostic: ListBuffer[Diagnostic] = parseResult.diagnostic
  var types: mutable.HashMap[String, TypeSymbol] = mutable.HashMap(
    "Boolean" -> BaseTypeSymbol("Boolean"),
    "Int32" -> BaseTypeSymbol("Int32"),
    "Int64" -> BaseTypeSymbol("Int64"),
    "UInt32" -> BaseTypeSymbol("UInt32"),
    "UInt64" -> BaseTypeSymbol("UInt64"),
    "Rune" -> BaseTypeSymbol("Rune"),
    "Iterable" -> null,
    "PrimitiveIntrinsics" -> null,
    "System" -> null,
    "Iterator" -> null,
    "Iterable" -> null,
    "RangeIterator" -> null,
    "Range" -> null,
    "ArrayIterator" -> null,
    "Array" -> null,
    "ArrayListIterator" -> null,
    "ArrayList" -> null,
    "String" -> null
  )

  var generics: mutable.HashMap[String, TypeParameterSymbol] = mutable.HashMap()


  override def root(): SyntaxNode = rootNode

  override def invalidRanges(): util.Collection[TextSpan] = parseResult.invalidRanges()

  override def diagnostics(): util.Collection[Diagnostic] = parseResult.diagnostics()

  override def lookupType(s: String): TypeSymbol = types(s)

  override def typeDefinitions(): util.List[_ <: TypeSymbol] =
    val x = this.preTypeDefinitions()
    this.code = 2
    this.preTypeDefinitions()

  def preTypeDefinitions(): util.List[_ <: TypeSymbol] =
    val list = ListBuffer[MyTypeSymbol]()
    for (i <- 0 until rootNode.slot(0).slotCount()) {
      list.append(checkTypeDefinition(rootNode.slot(0).slot(i)))
    }
    list.asJava

  def checkTypeDefinition(node: SyntaxNode): MyTypeSymbol =
    if (!types.contains(node.name))
      types += (node.name -> MyTypeSymbol(name = node.name, definition = node, kind = null))
    val symbol = MyTypeSymbol(
      kind = node.symbolKind,
      name = node.name,
      definition = node)
    symbol.typeArgs = getTypeArgs(node, symbol)
    symbol.baseTypesBuffer = getParents(node)
    symbol.memberSymbols = getMembers(node, symbol)
    symbol.isAbstract = checkAbstractness(node, symbol)
    types(symbol.name) = symbol
    symbol

  def getParents(node: SyntaxNode): ListBuffer[TypeSymbol] = node.parents.map(x => getNameExpression(x).asInstanceOf[TypeSymbol]) // TODO: Check

  def getTypeArgs(node: SyntaxNode, owner: SemanticSymbol): ListBuffer[TypeLikeSymbol] = node.typeArgs.map(x => getTypeParameterDefinition(x, owner))

  def getTypeParameterDefinition(node: SyntaxNode, owner: SemanticSymbol): TypeLikeSymbol =
    val param = MyTypeParameterSymbol(
      typeParamBounds = node.parents.map(getNameExpression), // TODO: ???? can be unexpected behavior
      owner = owner,
      kind = node.symbolKind,
      name = node.name,
      definition = node)
    if (generics.contains(node.name)) {
      generics(node.name) = param
    }
    generics += (node.name -> param)
    param




  def getNameExpression(node: SyntaxNode): TypeLikeSymbol =
    if (node == null) return null
    node.kind() match
      case SyntaxKind.IDENTIFIER_NAME_EXPRESSION |
           SyntaxKind.GENERIC_NAME_EXPRESSION |
           SyntaxKind.OPTION_NAME_EXPRESSION =>
        if (generics.contains(node.name)) return generics(node.name)
        if (!types.contains(node.name) && code == 0) {
          types += (node.name -> EmptyTypeSymbol(node.name))
          println("Type could be not declared below! Please check this!")
          // TODO: Check it in the end
        } else if ((!types.contains(node.name) && code == 2) || (types.contains(node.name) && types(node.name).isInstanceOf[EmptyTypeSymbol] && code == 2)) {
          throw RuntimeException(s"No such type declared in code ${node.name}")
        }
        else if (code == 2) {
          node.kind() match
            case SyntaxKind.GENERIC_NAME_EXPRESSION =>
              types(node.name).asInstanceOf[MyTypeSymbol].setEnvironment(types)
              return types(node.name).construct(node.genericParams.map(getNameExpression).asJava)
            case _ => return types(node.name)
        }
        types(node.name)
      case SyntaxKind.TYPE_DEFINITION => types(node.name)
      case _ => throw RuntimeException(s"Not a name expression in getNameExpression $node")

  def getMembers(node: SyntaxNode, owner: SemanticSymbol): ListBuffer[MemberSymbol] = node.definitions.map(x => getDefinition(x, owner))

  def getDefinition(node: SyntaxNode, owner: SemanticSymbol): MemberSymbol =
    node.kind() match
      case SyntaxKind.FUNCTION_DEFINITION =>
        val functionSymbol = MyFunctionSymbol(
          isNative = node.isNative,
          isVirtual = node.isVirtual(owner),
          isAbstract = node.isAbstract(owner),
          isOverride = node.isOverride,
          returnType = getNameExpression(node.returnType(owner)),
          owner = owner,
          kind = node.symbolKind,
          name = node.name,
          definition = node)
        functionSymbol.functionParameters = getFunctionParameters(node, functionSymbol)
        functionSymbol.functionLocals = getFunctionLocals(node, functionSymbol)
        functionSymbol
      case SyntaxKind.VARIABLE_DEFINITION => MyVariableSymbol(
        `type` = getNameExpression(node.`type`),
        owner = owner,
        kind = SymbolKind.FIELD,
        name = node.name,
        definition = node
      )
      case _ => throw RuntimeException("Not a variable or a function!")

  def getFunctionParameters(node: SyntaxNode, owner: SemanticSymbol): ListBuffer[VariableSymbol] = node.functionParameters.map(x => getParameterDefenition(x, owner))

  def getFunctionLocals(node: SyntaxNode, owner: SemanticSymbol): ListBuffer[VariableSymbol] = node.functionStatements.filter(x => x != null).flatMap(x => getLocal(x, owner))

  def getLocal(node: SyntaxNode, owner: SemanticSymbol): ListBuffer[VariableSymbol] =
    val result = ListBuffer[VariableSymbol]()
    node.kind() match
      case SyntaxKind.VARIABLE_DEFINITION_STATEMENT =>
        result.append(MyVariableSymbol(
          `type` = getNameExpression(node.variable.`type`),
          owner = owner,
          kind = node.variable.symbolKind,
          name = node.variable.name,
          definition = node.variable))
      case SyntaxKind.FOR_STATEMENT =>
        result.append(MyVariableSymbol(
          `type` = getNameExpression(node.forLocal.`type`),
          owner = owner,
          kind = node.forLocal.symbolKind,
          name = node.forLocal.name,
          definition = node
        ))
        result.addAll(node.forStatements.flatMap(x => getLocal(x, owner)))
      case SyntaxKind.EXPRESSION_STATEMENT if node.expression.kind() == SyntaxKind.IS_EXPRESSION =>
        result.append(MyVariableSymbol(
          `type` = types("Boolean"),
          owner = owner,
          kind = node.expression.slot(3).symbolKind,
          name = node.expression.slot(3).name,
          definition = node.expression.slot(3) // TODO:
        ))
      case _ => ListBuffer.empty

  // x: Int
  def getParameterDefenition(node: SyntaxNode, owner: SemanticSymbol): VariableSymbol = MyVariableSymbol(
    `type` = getNameExpression(node.`type`),
    owner = owner,
    kind = node.symbolKind,
    name = node.name,
    definition = node,
  )

  def checkAbstractness(node: SyntaxNode, symbol: TypeSymbol): Boolean =
    node.symbolKind match
      case SymbolKind.INTERFACE => true
      case SymbolKind.CLASS =>
        val symbolFunctions = symbol.members().asScala.filter(_.isInstanceOf[FunctionSymbol])
        val baseTypesAbstractFunctions: mutable.Buffer[MemberSymbol] = symbol.baseTypes().asScala.filter(x => x != null)
          .flatMap(_.members().asScala)
          .filter(x => x.isInstanceOf[FunctionSymbol] && x.asInstanceOf[FunctionSymbol].isAbstract)
        symbolFunctions.exists(x => x.asInstanceOf[FunctionSymbol].isAbstract) ||
          baseTypesAbstractFunctions
            .intersect(symbolFunctions.filter(!_.asInstanceOf[FunctionSymbol].isOverride)).nonEmpty
      case _ => false

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


  //def f(x):
  //    val array = List<int>(1, 2, 3, 4)
  //    for y in array
  //       val x = y + 1
  //    return x is SomeClass
  // FOR_STATEMENT
  // FOR IDENTIFIER(x) IN IDENTIFIER_NAME_EXPRESSION(array)
}