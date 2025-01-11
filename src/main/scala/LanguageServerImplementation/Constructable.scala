package LanguageServerImplementation

import LanguageServerImplementation.Context.{GenericEnvironment, TypeEnvironment}
import ParserImplementation.Parsing.{MyParseResult, MySyntaxNode}
import syspro.tm.parser.{SyntaxKind, SyntaxNode}
import syspro.tm.symbols.{TypeLikeSymbol, TypeParameterSymbol, TypeSymbol}

import java.util
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import Utils.*
import syspro.tm.lexer.IdentifierToken

trait Constructable {

  def constr(list: util.List[_ <: TypeLikeSymbol] , typeArgs: ListBuffer[TypeLikeSymbol], definition: SyntaxNode, `def`: TypeSymbol, types: TypeEnvironment): MyTypeSymbol = {
    val context = Context(types = types)
    context.push()
    val normalList = list.asScala


    if (normalList.length != typeArgs.length)
      throw RuntimeException(s"Different length of typeArgs ${typeArgs.length} and list ${normalList.length} def $definition ${`def`} ")

    var i = 0
    for (elem <- typeArgs) {
      normalList(i) match
        case t: TypeSymbol => context.add(elem.name(), t)  // TODO: Может стоит проверить есть ли в контектсе уже этот элемент
        case p: TypeParameterSymbol => context.push(elem.name(), p)
        case _  if !context.containsClass(elem.name) => context.push(elem.name, _)
        case _  if context.containsClass(elem.name) => ???
      i += 1
    }
    val rebuildNode = buildNewTree(definition, context)
    val node = MySyntaxNode(definition.kind())
    node.add(rebuildNode.slot(0))
    node.add(rebuildNode.slot(1))
    node.addFail(3)
    node.add(rebuildNode.slot(5))
    node.add(rebuildNode.slot(6))
    node.add(rebuildNode.slot(7))
    node.add(rebuildNode.slot(8))
    val model = MySemanticModel.emptyModel
    model.context.push()
    context.add(node.name, null)
    model.update(context)
//    model.types = model.types ++ types
//    model.generics = model.generics ++ generics
    model.code = 1
    val symbol = MyTypeSymbol(
      kind = node.symbolKind,
      name = node.name,
      baseTypesBuffer = model.getParents(node),
      definition = node,
      originalDef = `def`
    )
    symbol.typeArgs = ListBuffer(normalList.toSeq *)
    symbol.memberSymbols = model.getMembers(node, symbol)
    symbol.isAbstract = model.checkAbstractness(node, symbol)
    model.context(symbol.name) = symbol
    model.context.pop()
    symbol
  }

  private def buildNewTree(node: SyntaxNode, context: Context): SyntaxNode =
    if (node == null)
      return null
    val token = node.token()
    var newNode: MySyntaxNode = null
    var name = ""
    (node.kind(), token) match
      case (SyntaxKind.IDENTIFIER, identifierToken: IdentifierToken) =>
        (context.containsParam(identifierToken.value), context.containsClass(identifierToken.value)) match
          case (_, true) =>
            if (context.getClass(identifierToken.value) != null)
              name = context.getClass(identifierToken.value).name;
          case (true, _) =>
            if (context.getParameter(identifierToken.value) != null)
              name = context.getParameter(identifierToken.value).name;
          case (false, false) => name = identifierToken.value
        val newToken = IdentifierToken(token.start, token.end, token.leadingTriviaLength,  token.trailingTriviaLength, name, identifierToken.contextualKeyword)
        newNode = MySyntaxNode(node.kind(), newToken)
      case _ => newNode = MySyntaxNode(node.kind(), node.token())
    node.children.map(n => buildNewTree(n, context))
    newNode.children = node.children
    newNode
}
