package LanguageServerImplementation

import syspro.tm.symbols.{FunctionSymbol, LanguageServer, MemberSymbol, SemanticModel, SemanticSymbol, SymbolKind, SyntaxNodeWithSymbols, TypeLikeSymbol, TypeParameterSymbol, TypeSymbol, VariableSymbol}
import syspro.tm.parser.SyntaxKind.*
import syspro.tm.parser.SyntaxNode
import syspro.tm.lexer.Symbol
import syspro.tm.lexer.Symbol.*
import ParserImplementation.Parsing.{MyParseResult, MyParser, MySyntaxNode}
import syspro.tm.lexer.Keyword.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import Utils.*

class MyLanguageServer extends LanguageServer {
  private var nodes: mutable.HashMap[SyntaxNode, SemanticSymbol] = mutable.HashMap()


  private def fillVariable(value: java.util.List[? <: VariableSymbol]): Unit =
    value.asScala.foreach(x => {
      nodes += (x.definition() -> x)
    })


  private def fillMemberNodes(memberDefinitions: java.util.List[? <: MemberSymbol]): Unit =
    memberDefinitions.asScala.foreach(x => {
      nodes += (x.definition() -> x)
      x match
        case f: FunctionSymbol =>
          fillVariable(f.locals())
          fillVariable(f.parameters())
        case v: VariableSymbol =>
          nodes += (v.definition() -> v)
    })

  private def fillTypeArgsNodes(typeArguments: java.util.List[? <: TypeLikeSymbol]): Unit =
    typeArguments.asScala.foreach(x => {
      nodes += (x.definition() -> x)

    })

  private def fillParentNodes(parents: Seq[? <: TypeSymbol]): Unit =
    parents.foreach(x => {
      if (x != null)
        nodes += (x.definition() -> x)
    })

  private def fillNodes(typeDefinitions: Seq[? <: TypeSymbol]): Unit =
    typeDefinitions.foreach(x =>
      { nodes += (x.definition() -> x)
        fillMemberNodes(x.members())
        fillTypeArgsNodes(x.typeArguments())
        fillParentNodes(x.baseTypes().asScala.toSeq)
      })

  private def build(node: SyntaxNode): MySyntaxNode = {
    if (node == null) return null
    val symbol = if (nodes.contains(node)) nodes(node) else null
    node match
      case my: MySyntaxNode =>
        my.sym = symbol
        my.children.map(build)
        my
      case _ => MySyntaxNode(var1 = node.kind(), var2 = node.token(), sym = symbol, children = node.children.map(build))
  }
  

  override def buildModel(s: String): SemanticModel =
    nodes = mutable.HashMap()
    val p = MyParser().parse(s)
    val semanticModel = MySemanticModel(p, p.root())
    fillNodes(semanticModel.typeDefinitions().asScala.toSeq)
    semanticModel.rootNode = build(p.root())
    semanticModel
}  