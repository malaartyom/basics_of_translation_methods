package LanguageServerImplementation

import LanguageServerImplementation.Context.TypeEnvironment
import LanguageServerImplementation.LaunchType.{SECOND, ZERO}
import LanguageServerImplementation.Symbols.{MyFunctionSymbol, MyTypeParameterSymbol, MyTypeSymbol, MyVariableSymbol}
import LanguageServerImplementation.Utils.*
import ParserImplementation.Parsing.MyParseResult
import syspro.tm.parser.{Diagnostic, SyntaxKind, SyntaxNode, TextSpan}
import syspro.tm.symbols.*
import syspro.tm.{lexer, parser}

import java.util
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

object MySemanticModel {
  val emptyModel = MySemanticModel(MyParseResult(null, null), null)
}
class MySemanticModel(var parseResult: MyParseResult, var rootNode: SyntaxNode)
  extends SemanticModel {

  var launch: LaunchType = ZERO
  var ranges: ListBuffer[TextSpan] = parseResult.invalid_ranges
  var diagnostic: ListBuffer[Diagnostic] = ListBuffer(parseResult.diagnostics().asScala.toSeq *)
  var context: Context = Context()


  def update(context: Context): Unit = this.context += context

  override def root(): SyntaxNode = rootNode

  override def invalidRanges(): util.Collection[TextSpan] = parseResult.invalidRanges()

  override def diagnostics(): util.Collection[Diagnostic] = parseResult.diagnostics()

  override def lookupType(s: String): TypeSymbol = context.getClass(s)

  override def typeDefinitions(): util.List[? <: TypeSymbol] =
    val x = this.preTypeDefinitions()
    launch = SECOND
    this.preTypeDefinitions()

  def preTypeDefinitions(): util.List[? <: TypeSymbol] =
    val list = ListBuffer[MyTypeSymbol]()
    for (i <- 0 until rootNode.slot(0).slotCount()) {
      list.append(checkTypeDefinition(rootNode.slot(0).slot(i)))
    }
    list.asJava

  def checkTypeDefinition(node: SyntaxNode): MyTypeSymbol =
    if (!context.containsClass(node.name)) {
      context.add(node.name, MyTypeSymbol(name = node.name, definition = node, kind = null))
    }
    val symbol = MyTypeSymbol(
      kind = node.symbolKind,
      name = node.name,
      definition = node)
    symbol.typeArgs = getTypeArgs(node, symbol)
    symbol.baseTypesBuffer = getParents(node)
    symbol.memberSymbols = getMembers(node, symbol)
    symbol.isAbstract = checkAbstractness(node, symbol)
    context(symbol.name) = symbol
    context.pop()
    symbol

  def getParents(node: SyntaxNode): ListBuffer[TypeSymbol] = node.parents.map(x => getNameExpression(x).asInstanceOf[TypeSymbol]) // TODO: Check

  def getTypeArgs(node: SyntaxNode, owner: SemanticSymbol): ListBuffer[TypeLikeSymbol] =
    context.push()
    node.typeArgs.map(x => getTypeParameterDefinition(x, owner))

  def getTypeParameterDefinition(node: SyntaxNode, owner: SemanticSymbol): TypeLikeSymbol =
    val param = MyTypeParameterSymbol(
      owner = owner,
      kind = node.symbolKind,
      name = node.name,
      definition = node)
    context(node.name) = param
    param.typeParamBounds = node.parents.map(getNameExpression)
    context.getParameter(node.name)


  def getNameExpression(node: SyntaxNode): TypeLikeSymbol =
    if (node == null) return null
    node.kind() match
      case SyntaxKind.IDENTIFIER_NAME_EXPRESSION |
           SyntaxKind.GENERIC_NAME_EXPRESSION |
           SyntaxKind.OPTION_NAME_EXPRESSION =>
        if (context.containsParam(node.name)) return context.getParameter(node.name)
        if (!context.containsClass(node.name) && launch == ZERO) {
          context.add(node.name, null)
          println("Type could be not declared below! Please check this!")
          // TODO: Check it in the end
        } else if (!context.containsClass(node.name) || context.containsEmpty(node.name) && launch == SECOND) {
          parseResult.addDiagnostic(node.fullSpan(), 20, s"name: ${node.name}, node.kind: ${node.kind}")
          context.add(node.name, null)
        }
        else if (launch == SECOND) {
          node.kind() match
            case SyntaxKind.GENERIC_NAME_EXPRESSION if context.getClass(node.name) != null =>
              context.setEnvironment(node.name)
              return context.getClass(node.name).construct(node.genericParams.map(getNameExpression).asJava)
            case _ => return context.getClass(node.name)
        }
        context.getClass(node.name)
      case SyntaxKind.TYPE_DEFINITION => context.getClass(node.name)
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
      case SyntaxKind.VARIABLE_DEFINITION =>
        checkString(node)
        MyVariableSymbol(
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
          `type` = context.getClass("Boolean"),
          owner = owner,
          kind = node.expression.expressionIsIdentifier.symbolKind,
          name = node.expression.expressionIsIdentifier.name,
          definition = node.expression.expressionIsIdentifier
        ))
      case _ => ListBuffer.empty

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



  def checkString(node: SyntaxNode): Unit =
    if (node.expression != null)
      node.expression.kind() match
        case SyntaxKind.STRING_LITERAL_EXPRESSION if context.containsEmpty("String") && context.containsEmpty("Array") => parseResult.addDiagnostic(node.fullSpan(), 20, "String and Array")
        case _ =>

}