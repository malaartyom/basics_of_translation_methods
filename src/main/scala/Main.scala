package LexerImplementation

import LanguageServerImplementation.MyLanguageServer
import LanguageServerImplementation.Symbols.MyFunctionSymbol
import LexerImplementation.Processors.UnicodeProcessor
import ParserImplementation.Parsing.MyParser
import syspro.tm.{Tasks, WebServer}
import syspro.tm.lexer.Token
import syspro.tm.lexer.TestMode
import syspro.tm.lexer.TestLineTerminators.{CarriageReturnLineFeed, LineFeed, Mixed, Native}
import syspro.tm.symbols.FunctionSymbol

import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]): Unit = {
//    val lexer = Tokenizer()
//    var test = TestMode()

    //    test = test.repeated(true)
    //    test = test.strict(true)
    //    test = test.parallel(true)
    //    test = test.shuffled(true)
    //    test = test.forceLineTerminators(Mixed)
//        syspro.tm.Tasks.Lexer.registerSolution(lexer, test)
//
//
//
//    WebServer.start()
//    val parser = MyParser()
//    syspro.tm.Tasks.Parser.registerSolution(parser)
//    WebServer.waitForWebServerExit()

//    Tasks.addTestIncludeFilter("OperatorClass")
//    WebServer.start()
    val server = MyLanguageServer()
    syspro.tm.Tasks.LanguageServer.registerSolution(server)
//    WebServer.waitForWebServerExit()
//    print(MyFunctionSymbol(isNative = false, isVirtual = false, isAbstract = false, isOverride = false, functionParameters = ListBuffer.empty, returnType = null, functionLocals = ListBuffer.empty, owner = null, kind = null, name = "bro", definition = null).isInstanceOf[FunctionSymbol])
  }

  private def printTokens(l: java.util.List[Token]): Unit = {
    var i: Int = 0
    (0 until l.size())
      .foreach(i =>
        print(i.toString + " " +
          l.get(i).toString + " " + l.get(i).start.toString + " " + l.get(i).end.toString + " " + l.get(i).leadingTriviaLength.toString + " " + l.get(i).trailingTriviaLength + "\n")
      )
  }
}

