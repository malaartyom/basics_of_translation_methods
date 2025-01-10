package LanguageServerImplementation

import ParserImplementation.Parsing.{MyParseResult, MySyntaxNode}
import syspro.tm.parser.{SyntaxKind, SyntaxNode}
import syspro.tm.symbols.{TypeLikeSymbol, TypeParameterSymbol, TypeSymbol}

import java.util
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import SyntaxNodeExtension.*
import syspro.tm.lexer.IdentifierToken

trait Constructable {

  type TypeEnvironment = scala.collection.mutable.HashMap[String, TypeSymbol]
  type GenericEnvironment = scala.collection.mutable.HashMap[String, TypeParameterSymbol]

  def constr(list: util.List[_ <: TypeLikeSymbol] , typeArgs: ListBuffer[TypeLikeSymbol], definition: SyntaxNode, `def`: TypeSymbol, types: TypeEnvironment): MyTypeSymbol = {
    val generics: GenericEnvironment = scala.collection.mutable.HashMap[String, TypeParameterSymbol]()
    val normalList = list.asScala


    if (normalList.length != typeArgs.length)
      throw RuntimeException(s"Different length of typeArgs ${typeArgs.length} and list ${normalList.length} def $definition ${`def`} ")

    var i = 0
    for (elem <- typeArgs) {
      normalList(i) match
        case t: TypeSymbol => types += (elem.name() -> t)
        case p: TypeParameterSymbol => generics += (elem.name() -> p)
      i += 1
    }
    val rebuildNode = buildNewTree(definition, types, generics)
    val node = MySyntaxNode(definition.kind())
    node.add(rebuildNode.slot(0))
    node.add(rebuildNode.slot(1))
    node.addFail(3)
    node.add(rebuildNode.slot(5))
    node.add(rebuildNode.slot(6))
    node.add(rebuildNode.slot(7))
    node.add(rebuildNode.slot(8))
    val model = MySemanticModel(MyParseResult(null, null), null)
    model.types += (node.name -> null)
    model.types = model.types ++ types
    model.generics = model.generics ++ generics
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
    model.types(symbol.name) = symbol
    symbol
  }

  private def buildNewTree(node: SyntaxNode, env: TypeEnvironment, generics: GenericEnvironment): SyntaxNode =
    if (node == null)
      return null
    val token = node.token()
    var newNode: MySyntaxNode = null
    var name = ""
    (node.kind(), token) match
      case (SyntaxKind.IDENTIFIER, identifierToken: IdentifierToken) =>
        (generics.contains(identifierToken.value), env.contains(identifierToken.value)) match
          case (_, true) => name = env(identifierToken.value).name;
          case (true, _) => name = generics(identifierToken.value).name;
          case (false, false) => name = identifierToken.value
        val newToken = IdentifierToken(token.start, token.end, token.leadingTriviaLength,  token.trailingTriviaLength, name, identifierToken.contextualKeyword)
        newNode = MySyntaxNode(node.kind(), newToken)
      case _ => newNode = MySyntaxNode(node.kind(), node.token())
    node.children.map(n => buildNewTree(n, env, generics))
    newNode.children = node.children
    newNode
}
