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
import SyntaxNodeExtension.*

class MyLanguageServer extends LanguageServer {
  val nodes: mutable.HashMap[SyntaxNode, SemanticSymbol] = mutable.HashMap()


  def fillVariable(value: java.util.List[? <: VariableSymbol]): Unit =
    value.asScala.foreach(x => {
      nodes += (x.definition() -> x)
    })


  def fillMemeberNodes(memberDefinitions: java.util.List[? <: MemberSymbol]): Unit =
    memberDefinitions.asScala.foreach(x => {
      nodes += (x.definition() -> x)
      x match
        case f: FunctionSymbol =>
          fillVariable(f.locals())
          fillVariable(f.parameters())
        case v: VariableSymbol =>
          nodes += (v.definition() -> v)
    })

  def fillTypeArgsNodes(typeArguments: java.util.List[? <: TypeLikeSymbol]): Unit =
    typeArguments.asScala.foreach(x => {
      nodes += (x.definition() -> x)
//      x match
//        case p: TypeParameterSymbol => p.bounds() ???
    })

  def fillNodes(typeDefinitions: Seq[? <: TypeSymbol]): Unit =
    typeDefinitions.foreach(x =>
      { nodes += (x.definition() -> x)
        fillMemeberNodes(x.members())
        fillTypeArgsNodes(x.typeArguments())
        fillNodes(x.baseTypes().asScala.toSeq)
      })

  def build(node: SyntaxNode): MySyntaxNodeWithSymbol =
    MySyntaxNodeWithSymbol(node = node,
      nodeSymbol = nodes(node), children = node.children.map(build))

  override def buildModel(s: String): SemanticModel =
    val p = MyParser().parse(s)
    val semanticModel = MySemanticModel(p, p.root())
    fillNodes(semanticModel.typeDefinitions().asScala.toSeq)
    println(nodes)
    MySemanticModel(p, build(p.root()))
}  