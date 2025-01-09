package LanguageServerImplementation

import ParserImplementation.Parsing.{MyParseResult, MySyntaxNode}
import syspro.tm.parser.{SyntaxKind, SyntaxNode}
import syspro.tm.symbols.{TypeLikeSymbol, TypeSymbol}

import java.util
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import SyntaxNodeExtension.*
import syspro.tm.lexer.IdentifierToken

trait Constructable {

  type Environment = scala.collection.mutable.HashMap[String, TypeSymbol]

  def constr(list: util.List[_ <: TypeLikeSymbol] , typeArgs: ListBuffer[TypeLikeSymbol], definition: SyntaxNode, `def`: TypeSymbol): MyTypeSymbol = {
    val normalList = list.asScala
    if (normalList.length != typeArgs.length)
      throw RuntimeException(s"Different length of typeArgs ${typeArgs.length} and list ${normalList.length} def $definition ${`def`} ")

    val env: Environment = scala.collection.mutable.HashMap[String, TypeSymbol]()
    var i = 0
    for (elem <- typeArgs) {
      env += (elem.name() -> normalList(i).asInstanceOf[TypeSymbol])
      i += 1
    }
    val rebuildNode = buildNewTree(definition, env)
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
    model.types = model.types ++ env
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

  private def buildNewTree(node: SyntaxNode, env: Environment): SyntaxNode =
    if (node == null) return null
    var newNode = MySyntaxNode(node.kind(), node.token())
    if (node.kind() == SyntaxKind.IDENTIFIER && env.contains(node.token().asInstanceOf[IdentifierToken].value)) {
      val oldToken = node.token()
      val newToken = IdentifierToken(oldToken.start, oldToken.end, oldToken.leadingTriviaLength,
        oldToken.trailingTriviaLength, env(node.token().asInstanceOf[IdentifierToken].value).name(), oldToken.asInstanceOf[IdentifierToken].contextualKeyword)
      newNode = MySyntaxNode(node.kind(), newToken)
    }
    node.children.map(n => buildNewTree(n, env))
    newNode.children = node.children
    newNode
}
