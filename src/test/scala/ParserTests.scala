import MyLexer.Tokenizer
import MyParser.MySyntaxNode

import syspro.tm.lexer.{IdentifierToken, SymbolToken, Symbol, Keyword}
import syspro.tm.parser.{SyntaxKind, AnySyntaxKind}
import scala.jdk.CollectionConverters.*


class ParserTests extends munit.FunSuite {
  test("General Parser Tests") {
    val t = Tokenizer()
    val s = "class Identifier0<T <: U & V, G <: X & Y>  <: Object0"
    val myParser = MyParser.MyParser()
    val r = myParser.parse(s)
    val firstTypeDef = r.root().slot(0)

    assertEquals(r.root().kind(), SyntaxKind.SOURCE_TEXT)
    assertEquals(firstTypeDef.kind, SyntaxKind.TYPE_DEFINITION)
    assertEquals(firstTypeDef.slot(0).kind(), Keyword.CLASS)
    assertEquals(firstTypeDef.slot(1).kind(), SyntaxKind.IDENTIFIER)
    assertEquals(firstTypeDef.slot(2).kind(), Symbol.LESS_THAN)
    assertEquals(firstTypeDef.slot(3).kind(), SyntaxKind.SEPARATED_LIST)

    val sepList = firstTypeDef.slot(3)
    assertEquals(sepList.slot(0).kind(), SyntaxKind.TYPE_PARAMETER_DEFINITION)

    val firstTypeParamDef = sepList.slot(0)

    assertEquals(firstTypeParamDef.slot(0).kind(), SyntaxKind.IDENTIFIER)
    assertEquals(firstTypeParamDef.slot(1).kind(), SyntaxKind.TYPE_BOUND)

    val firstTypeBound = firstTypeParamDef.slot(1)

    assertEquals(firstTypeBound.slot(0).kind(), Symbol.BOUND)
    assertEquals(firstTypeBound.slot(1).kind(), SyntaxKind.SEPARATED_LIST)


    val separatedListTypeBound = firstTypeBound.slot(1)

    assertEquals(separatedListTypeBound.slot(0).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)
    assertEquals(separatedListTypeBound.slot(1).kind(), Symbol.AMPERSAND)
    assertEquals(separatedListTypeBound.slot(2).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)

    //    val sepList1 = firstTypeDef.slot(1)
    //
    //    assertEquals(sepList1.slot(0).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)


    assertEquals(sepList.slot(1).kind(), Symbol.COMMA)
    assertEquals(sepList.slot(2).kind(), SyntaxKind.TYPE_PARAMETER_DEFINITION)

    assertEquals(firstTypeDef.slot(4).kind(), Symbol.GREATER_THAN)
    assertEquals(firstTypeDef.slot(5).kind(), SyntaxKind.TYPE_BOUND)


  }

  test("NameExpression") {

    val t = Tokenizer()
    val s = "name0< ?name1, name2>"
    val myParser = MyParser.MyParser()
    val r = t.lex(s)
    println()
    val nameExpr = myParser.matchNameExpression(r.asScala.toVector)

    assertEquals(nameExpr.kind(), SyntaxKind.GENERIC_NAME_EXPRESSION)

    assertEquals(nameExpr.slot(0).kind(), SyntaxKind.IDENTIFIER)
    assertEquals(nameExpr.slot(1).kind(), Symbol.LESS_THAN)
    assertEquals(nameExpr.slot(2).kind(), SyntaxKind.SEPARATED_LIST)

    val sepList = nameExpr.slot(2)
    assertEquals(sepList.slot(0).kind(), SyntaxKind.OPTION_NAME_EXPRESSION)
    assertEquals(sepList.slot(1).kind(), Symbol.COMMA)
    assertEquals(sepList.slot(2).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)


    assertEquals(nameExpr.slot(3).kind(), Symbol.GREATER_THAN)

  }

  test("Func Definition") {
    val t = Tokenizer()
    val s = "override abstract virtual native def f()\n  break"
    val tokens = t.lex(s)


    val p = MyParser.MyParser()
    val r = p.matchFuncDef(tokens.asScala.toVector)

    assertEquals(r.kind(), SyntaxKind.FUNCTION_DEFINITION)
    // TODO: Tests for defenitions
  }

  test("Statements") {

  }
}

class TypeParamDefinitionTests extends munit.FunSuite {
  test("Simple TypeParamDefinitionTests") {
    val t = Tokenizer()
    val s = "func"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchTypeParamDef(tokens.asScala.toVector)

    assertEquals(r.kind(), SyntaxKind.TYPE_PARAMETER_DEFINITION)
    assertEquals(r.slotCount(), 1)
    assertEquals(r.slot(0).kind(), SyntaxKind.IDENTIFIER)


  }

  test("Basic TypeParamDefinitionTests") {
    val t = Tokenizer()
    val s = "T <: U & V & G"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchTypeParamDef(tokens.asScala.toVector)

    assertEquals(r.kind(), SyntaxKind.TYPE_PARAMETER_DEFINITION)
    assertEquals(r.slotCount(), 2)
    assertEquals(r.slot(0).kind(), SyntaxKind.IDENTIFIER)
    assertEquals(r.slot(1).kind(), SyntaxKind.TYPE_BOUND)

    val typeBound = r.slot(1)

    assertEquals(typeBound.slotCount(), 2)
    assertEquals(typeBound.slot(0).kind(), Symbol.BOUND)
    assertEquals(typeBound.slot(1).kind(), SyntaxKind.SEPARATED_LIST)

    val sepList = typeBound.slot(1)

    assertEquals(sepList.slotCount(), 5)
    assertEquals(sepList.slot(0).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)
    assertEquals(sepList.slot(1).kind(), Symbol.AMPERSAND)
    assertEquals(sepList.slot(2).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)
    assertEquals(sepList.slot(3).kind(), Symbol.AMPERSAND)
    assertEquals(sepList.slot(4).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)
  }

  test("Complicated TypeParamDefinitionTests") {
    val t = Tokenizer()
    val s = "T <: U<X, Y> & ?V & G"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchTypeParamDef(tokens.asScala.toVector)

    assertEquals(r.kind(), SyntaxKind.TYPE_PARAMETER_DEFINITION)
    assertEquals(r.slotCount(), 2)
    assertEquals(r.slot(0).kind(), SyntaxKind.IDENTIFIER)
    assertEquals(r.slot(1).kind(), SyntaxKind.TYPE_BOUND)

    val typeBound = r.slot(1)

    assertEquals(typeBound.slotCount(), 2)
    assertEquals(typeBound.slot(0).kind(), Symbol.BOUND)
    assertEquals(typeBound.slot(1).kind(), SyntaxKind.SEPARATED_LIST)

    val sepList = typeBound.slot(1)

    assertEquals(sepList.slotCount(), 5)
    assertEquals(sepList.slot(0).kind(), SyntaxKind.GENERIC_NAME_EXPRESSION)
    assertEquals(sepList.slot(1).kind(), Symbol.AMPERSAND)
    assertEquals(sepList.slot(2).kind(), SyntaxKind.OPTION_NAME_EXPRESSION)
    assertEquals(sepList.slot(3).kind(), Symbol.AMPERSAND)
    assertEquals(sepList.slot(4).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)
  }

}

class ParamDefinitionTests extends munit.FunSuite {
  test("ParamDefinitionTest") {

    val t = Tokenizer()
    val s = "function: name0< ?name1, name2, ?name3>"
    val tokens = t.lex(s)


    val p = MyParser.MyParser()
    val r = p.matchParamDef(tokens.asScala.toVector)

    assertEquals(r.kind(), SyntaxKind.PARAMETER_DEFINITION)
    assertEquals(r.slotCount(), 3)
    assertEquals(r.slot(0).kind(), SyntaxKind.IDENTIFIER)
    assertEquals(r.slot(1).kind(), Symbol.COLON)
    assertEquals(r.slot(2).kind(), SyntaxKind.GENERIC_NAME_EXPRESSION)

  }
}


class FunctionDefinitionsTests extends munit.FunSuite {
  test("Simple Definition") {
    val t = Tokenizer()
    val s = "override abstract virtual native def f()\n  break"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchFuncDef(tokens.asScala.toVector)

    assertEquals(r.kind(), SyntaxKind.FUNCTION_DEFINITION)
    assertEquals(r.slotCount(), 11)
    assertEquals(r.slot(0).kind(), Keyword.OVERRIDE)
    assertEquals(r.slot(1).kind(), Keyword.ABSTRACT)
    assertEquals(r.slot(2).kind(), Keyword.VIRTUAL)
    assertEquals(r.slot(3).kind(), Keyword.NATIVE)
    assertEquals(r.slot(4).kind(), Keyword.DEF)
    assertEquals(r.slot(5).kind(), SyntaxKind.IDENTIFIER)
    assertEquals(r.slot(6).kind(), Symbol.OPEN_PAREN)
    assertEquals(r.slot(7).kind(), Symbol.CLOSE_PAREN)
    assertEquals(r.slot(8).kind(), SyntaxKind.INDENT)
    assertEquals(r.slot(9).kind(), SyntaxKind.LIST)
    assertEquals(r.slot(10).kind(), SyntaxKind.DEDENT)

    // TODO: Tests for defenitions
  }


  test("Complicated Definition") {
    val t = Tokenizer()
    val s = "override abstract virtual native def f(x: name0, y: ?name1, z: name3<T>): name5<V> \n  break\n  continue\n  "
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchFuncDef(tokens.asScala.toVector)

    assertEquals(r.kind(), SyntaxKind.FUNCTION_DEFINITION)
    assertEquals(r.slotCount(), 14)
    assertEquals(r.slot(0).kind(), Keyword.OVERRIDE)
    assertEquals(r.slot(1).kind(), Keyword.ABSTRACT)
    assertEquals(r.slot(2).kind(), Keyword.VIRTUAL)
    assertEquals(r.slot(3).kind(), Keyword.NATIVE)
    assertEquals(r.slot(4).kind(), Keyword.DEF)
    assertEquals(r.slot(5).kind(), SyntaxKind.IDENTIFIER)
    assertEquals(r.slot(6).kind(), Symbol.OPEN_PAREN)
    assertEquals(r.slot(7).kind(), SyntaxKind.SEPARATED_LIST)
    assertEquals(r.slot(8).kind(), Symbol.CLOSE_PAREN)
    assertEquals(r.slot(9).kind(), Symbol.COLON)
    assertEquals(r.slot(10).kind(), SyntaxKind.GENERIC_NAME_EXPRESSION)
    assertEquals(r.slot(11).kind(), SyntaxKind.INDENT)
    assertEquals(r.slot(12).kind(), SyntaxKind.LIST)
    assertEquals(r.slot(13).kind(), SyntaxKind.DEDENT)
  }
}

class VariableDefinitionTests extends munit.FunSuite {
  test("VarDef (1) ") {
    val t = Tokenizer()
    val s = "var x"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchVariableDef(tokens.asScala.toVector)
    
    assertEquals(r.kind(), SyntaxKind.VARIABLE_DEFINITION)
    assertEquals(r.slotCount(), 2)
    assertEquals(r.slot(0).kind(), Keyword.VAR)
    assertEquals(r.slot(1).kind(), SyntaxKind.IDENTIFIER)

  }

  test("VarDef (2) ") {
    val t = Tokenizer()
    val s = "val x"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchVariableDef(tokens.asScala.toVector)

    assertEquals(r.kind(), SyntaxKind.VARIABLE_DEFINITION)
    assertEquals(r.slotCount(), 2)
    assertEquals(r.slot(0).kind(), Keyword.VAL)
    assertEquals(r.slot(1).kind(), SyntaxKind.IDENTIFIER)

  }

  test("VarDef (3) ") {
    val t = Tokenizer()
    val s = "val x: name<T> =  name0"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchVariableDef(tokens.asScala.toVector)

    assertEquals(r.kind(), SyntaxKind.VARIABLE_DEFINITION)
    assertEquals(r.slotCount(), 6)
    assertEquals(r.slot(0).kind(), Keyword.VAL)
    assertEquals(r.slot(1).kind(), SyntaxKind.IDENTIFIER)
    assertEquals(r.slot(2).kind(), Symbol.COLON)
    assertEquals(r.slot(3).kind(), SyntaxKind.GENERIC_NAME_EXPRESSION)
    assertEquals(r.slot(4).kind(), Symbol.EQUALS)
    assertEquals(r.slot(5).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)
    

  }
}

class StatementTests extends munit.FunSuite {
  test("break") {

    val t = Tokenizer()
    val s = "break"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchStatement(tokens.asScala.toVector)
    
    assertEquals(r.slotCount(), 1)
    assertEquals(r.kind(), SyntaxKind.BREAK_STATEMENT)
    assertEquals(r.slot(0).kind(), Keyword.BREAK)
    
  }
  
  test("continue") {

    val t = Tokenizer()
    val s = "continue"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchStatement(tokens.asScala.toVector)

    assertEquals(r.slotCount(), 1)
    assertEquals(r.kind(), SyntaxKind.CONTINUE_STATEMENT)
    assertEquals(r.slot(0).kind(), Keyword.CONTINUE)
    
  }
  
  
  test ("return") {
    val t = Tokenizer()
    val s = "return"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchStatement(tokens.asScala.toVector)

    assertEquals(r.slotCount(), 1)
    assertEquals(r.kind(), SyntaxKind.RETURN_STATEMENT)
    assertEquals(r.slot(0).kind(), Keyword.RETURN)
  }
  
  test("return x") {
    val t = Tokenizer()
    val s = "return x"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchStatement(tokens.asScala.toVector)

    assertEquals(r.slotCount(), 2)
    assertEquals(r.kind(), SyntaxKind.RETURN_STATEMENT)
    assertEquals(r.slot(0).kind(), Keyword.RETURN)
    assertEquals(r.slot(1).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)
  }
  
  test("expression statement") {
    val t = Tokenizer()
    val s = "identifier"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchStatement(tokens.asScala.toVector)

    assertEquals(r.slotCount(), 1)
    assertEquals(r.kind(), SyntaxKind.EXPRESSION_STATEMENT)
    assertEquals(r.slot(0).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)
  }

  test("assignment statement") {

    val t = Tokenizer()
    val s = "name0 = name1<T>"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchStatement(tokens.asScala.toVector)
    assertEquals(r.kind(), SyntaxKind.ASSIGNMENT_STATEMENT)
    assertEquals(r.slotCount(), 3)
    assertEquals(r.slot(0).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)
    assertEquals(r.slot(1).kind(), Symbol.EQUALS)
    assertEquals(r.slot(2).kind(), SyntaxKind.GENERIC_NAME_EXPRESSION)
  }
  
  test("variable definition statement") {

    val t = Tokenizer()
    val s = "var x = name1"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchStatement(tokens.asScala.toVector)
    
    assertEquals(r.kind(), SyntaxKind.VARIABLE_DEFINITION_STATEMENT)
    assertEquals(r.slotCount(), 1)
    assertEquals(r.slot(0).kind(), SyntaxKind.VARIABLE_DEFINITION)
    
    
  }
  
  test("if statement") {
    val t = Tokenizer()
    val s = "if x\n  break\n  continue\n  return\nelse\n  break\n  continue\n  return"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchStatement(tokens.asScala.toVector)

    assertEquals(r.kind(), SyntaxKind.IF_STATEMENT)
    assertEquals(r.slotCount(), 9)
    assertEquals(r.slot(0).kind(), Keyword.IF)
    assertEquals(r.slot(1).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)
    assertEquals(r.slot(2).kind(), SyntaxKind.INDENT)
    assertEquals(r.slot(3).kind(), SyntaxKind.LIST)
    assertEquals(r.slot(4).kind(), SyntaxKind.DEDENT)
    assertEquals(r.slot(5).kind(), Keyword.ELSE)
    assertEquals(r.slot(6).kind(), SyntaxKind.INDENT)
    assertEquals(r.slot(7).kind(), SyntaxKind.LIST)
    assertEquals(r.slot(8).kind(), SyntaxKind.DEDENT)
  }
  
  test("while statement") {
    val t = Tokenizer()
    val s = "while x\n  break\n  continue\n  return\n"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchStatement(tokens.asScala.toVector)

    assertEquals(r.slotCount(), 5)
    assertEquals(r.slot(0).kind(), Keyword.WHILE)
    assertEquals(r.slot(1).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)
    assertEquals(r.slot(2).kind(), SyntaxKind.INDENT)
    assertEquals(r.slot(3).kind(), SyntaxKind.LIST)
    assertEquals(r.slot(4).kind(), SyntaxKind.DEDENT)

  }
  
  test("for statement") {

    val t = Tokenizer()
    val s = "for x in a\n  break\n  continue\n  return\n"
    val tokens = t.lex(s)

    val p = MyParser.MyParser()
    val r = p.matchStatement(tokens.asScala.toVector)

    assertEquals(r.slotCount(), 7)
    assertEquals(r.slot(0).kind(), Keyword.FOR)
    assertEquals(r.slot(1).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)
    assertEquals(r.slot(2).kind(), Keyword.IN)
    assertEquals(r.slot(3).kind(), SyntaxKind.IDENTIFIER_NAME_EXPRESSION)
    assertEquals(r.slot(4).kind(), SyntaxKind.INDENT)
    assertEquals(r.slot(5).kind(), SyntaxKind.LIST)
    assertEquals(r.slot(6).kind(), SyntaxKind.DEDENT)
    
    
  }


  
}
