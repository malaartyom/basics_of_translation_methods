package LanguageServerImplementation

import ParserImplementation.MyParser
import syspro.tm.symbols.{LanguageServer, SemanticModel}


class MyLanguageServer extends LanguageServer {
  override def buildModel(s: String): SemanticModel =
    val parseResult = MyParser().parse(s)
    val semanticModel = MySemanticModel(rootNode = parseResult.root())
    // TODO: ???
    semanticModel
}
