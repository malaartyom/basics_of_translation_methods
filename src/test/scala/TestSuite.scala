// For more information on writing tests, see

import scala.collection.JavaConverters.seqAsJavaListConverter

import MyLexer.Tokenizer
import syspro.tm.lexer.*
// https://scalameta.org/munit/docs/getting-started.html

var l = Tokenizer()

class SimpleTests extends munit.FunSuite {
  test("Simple identifier") {
    val code = "abc"
    val actual = l.lex(code)
    val expected = List[Token](IdentifierToken(0, 2, 0, 0, "abc", null))

    val triviaExpected = code.count(x => x == ' ')
    var triviaCurrent = 0

    var counter = 0

    for (i <- 0 to expected.length - 1) {
      val token = actual.get(i)
      val result = equalIdentifierTokens(token, expected(i))
      assert(result(0), result(1))
      triviaCurrent += token.leadingTriviaLength + token.trailingTriviaLength
      assertEquals(token.start, counter)
      counter = token.end + 1
    }
    assertEquals(code.length(), counter)
  }

  test("Two simple identifiers") {
    val code = "abc cdef"
    val actual = l.lex(code)
    val expected = List[Token](IdentifierToken(0, 3, 0, 1, "abc", null), IdentifierToken(4, 7, 0, 0, "cdef", null))

    val triviaExpected = code.count(x => x == ' ')
    var triviaCurrent = 0

    var counter = 0

    for (i <- 0 to expected.length - 1) {
      val token = actual.get(i)
      val result = equalIdentifierTokens(token, expected(i))
      assert(result(0), result(1))
      triviaCurrent += token.leadingTriviaLength + token.trailingTriviaLength
      assertEquals(token.start, counter)
      counter = token.end + 1
    }
    assertEquals(code.length(), counter)
  }

  test("Simple strange identifiers 1") {
    val code = "ab1c _cdef"
    val actual = l.lex(code)
    val expected = List[Token](IdentifierToken(0, 4, 0, 1, "ab1c", null), IdentifierToken(5, 9, 0, 0, "_cdef", null))

    val triviaExpected = code.count(x => x == ' ')
    var triviaCurrent = 0

    var counter = 0

    for (i <- 0 to expected.length - 1) {
      val token = actual.get(i)
      val result = equalIdentifierTokens(token, expected(i))
      assert(result(0), result(1))
      triviaCurrent += token.leadingTriviaLength + token.trailingTriviaLength
      assertEquals(token.start, counter)
      counter = token.end + 1
    }
    assertEquals(code.length(), counter)
  }

  test("Simple strange identifiers 2") {
    val code = "ab1ࠪc _cdࠖef a࡙d"
    val actual = l.lex(code)
    val expected = List[Token](
      IdentifierToken(0, 5, 0, 1, "ab1ࠪc", null),
      IdentifierToken(6, 12, 0, 1, "_cdࠖef", null),
      IdentifierToken(13, 15, 0, 0, "a࡙d", null)
    )

    val triviaExpected = code.count(x => x == ' ')
    var triviaCurrent = 0

    var counter = 0

    for (i <- 0 to expected.length - 1) {
      val token = actual.get(i)
      val result = equalIdentifierTokens(token, expected(i))
      assert(result(0), result(1))
      triviaCurrent += token.leadingTriviaLength + token.trailingTriviaLength
      assertEquals(token.start, counter)
      counter = token.end + 1
    }
    assertEquals(code.length(), counter)
  }

  test("Simple contextual keyword") {
    val code = "class interface object null"
    val actual = l.lex(code)
    val expected = List[Token](
      IdentifierToken(0, 5, 0, 1, "class", Keyword.CLASS),
      IdentifierToken(6, 15, 0, 1, "interface", Keyword.INTERFACE),
      IdentifierToken(16, 22, 0, 1, "object", Keyword.OBJECT),
      IdentifierToken(23, 26, 0, 0, "null", Keyword.NULL)
    )
    val triviaExpected = code.count(x => x == ' ')
    var triviaCurrent = 0

    var counter = 0

    for (i <- 0 to expected.length - 1) {
      val token = actual.get(i)
      val result = equalIdentifierTokens(token, expected(i))
      assert(result(0), result(1))
      triviaCurrent += token.leadingTriviaLength + token.trailingTriviaLength
      assertEquals(token.start, counter)
      counter = token.end + 1
    }
    assertEquals(code.length(), counter)
  }
}

class SimpleKeywordsTest extends munit.FunSuite {
  test("Simple keyword") {
    val code = "if is else continue override"
    val actual = l.lex(code)
    val expected = List[Token](
      KeywordToken(0, 2, 0, 1, Keyword.IF),
      KeywordToken(3, 5, 0, 1, Keyword.IS),
      KeywordToken(6, 10, 0, 1, Keyword.ELSE),
      KeywordToken(11, 19, 0, 1, Keyword.CONTINUE),
      KeywordToken(20, 27, 0, 0, Keyword.OVERRIDE)
    )
    val triviaExpected = code.count(x => x == ' ')
    var triviaCurrent = 0

    var counter = 0

    for (i <- 0 to expected.length - 1) {
      val token = actual.get(i)
      val result = equalKeywordTokens(token, expected(i))
      assert(result(0), result(1))
      triviaCurrent += token.leadingTriviaLength + token.trailingTriviaLength
      assertEquals(token.start, counter)
      counter = token.end + 1
    }
    assertEquals(code.length(), counter)
  }
  // TODO: ADD MORE
}

class SimpleSymbolTest extends munit.FunSuite {
  test("Simple symbol") {
    val code = ". : , + - * / % ! ~ & | && || ^ < <= > >= << >> [ ] ( ) = == != ? <:"
    val actual = l.lex(code)
    val expected = List[Token](
      SymbolToken(0, 1, 0, 1, Symbol.DOT),
      SymbolToken(2, 3, 0, 1, Symbol.COLON),
      SymbolToken(4, 5, 0, 1, Symbol.COMMA),
      SymbolToken(6, 7, 0, 1, Symbol.PLUS),
      SymbolToken(8, 9, 0, 1, Symbol.MINUS),
      SymbolToken(10, 11, 0, 1, Symbol.ASTERISK),
      SymbolToken(12, 13, 0, 1, Symbol.SLASH),
      SymbolToken(14, 15, 0, 1, Symbol.PERCENT),
      SymbolToken(16, 16, 0, 0, Symbol.EXCLAMATION),
      SymbolToken(17, 19, 1, 1, Symbol.TILDE),
      SymbolToken(20, 20, 0, 0, Symbol.AMPERSAND),
      SymbolToken(21, 22, 1, 0, Symbol.BAR),
      SymbolToken(23, 25, 1, 0, Symbol.AMPERSAND_AMPERSAND),
      SymbolToken(26, 28, 1, 0, Symbol.BAR_BAR),
      SymbolToken(29, 31, 1, 1, Symbol.CARET),
      SymbolToken(32, 32, 0, 0, Symbol.LESS_THAN),
      SymbolToken(33, 35, 1, 0, Symbol.LESS_THAN_EQUALS),
      SymbolToken(36, 37, 1, 0, Symbol.GREATER_THAN),
      SymbolToken(38, 40, 1, 0, Symbol.GREATER_THAN_EQUALS),
      SymbolToken(41, 43, 1, 0, Symbol.LESS_THAN_LESS_THAN),
      SymbolToken(44, 46, 1, 0, Symbol.GREATER_THAN_GREATER_THAN),
      SymbolToken(47, 49, 1, 1, Symbol.OPEN_BRACKET),
      SymbolToken(50, 51, 0, 1, Symbol.CLOSE_BRACKET),
      SymbolToken(52, 53, 0, 1, Symbol.OPEN_PAREN),
      SymbolToken(54, 55, 0, 1, Symbol.CLOSE_PAREN),
      SymbolToken(56, 56, 0, 0, Symbol.EQUALS),
      SymbolToken(57, 59, 1, 0, Symbol.EQUALS_EQUALS),
      SymbolToken(60, 62, 1, 0, Symbol.EXCLAMATION_EQUALS),
      SymbolToken(63, 65, 1, 1, Symbol.QUESTION),
      SymbolToken(66, 67, 0, 0, Symbol.BOUND)
    )
    val triviaExpected = code.count(x => x == ' ')
    var triviaCurrent = 0

    var counter = 0

    for (i <- 0 to expected.length - 1) {
      val token = actual.get(i)
      val result = equalSymbolTokens(token, expected(i))
      assert(result(0), result(1))
      triviaCurrent += token.leadingTriviaLength + token.trailingTriviaLength
      assertEquals(token.start, counter, "Error Token: " + token.toString())
      counter = token.end + 1
    }
    assertEquals(code.length(), counter)
    assertEquals(triviaCurrent, triviaExpected)
  }
}

class SimpleIntegralLiteralTokenTest extends munit.FunSuite {
  test("Simple integer i") {
    val code = "1235 23434i32 323i64 123a23"
    val actual = l.lex(code)
    val expected = List[Token](
      IntegerLiteralToken(0, 4, 0, 1, BuiltInType.INT64, false, 1235),
      IntegerLiteralToken(5, 13, 0, 0, BuiltInType.INT32, true, 23434),
      IntegerLiteralToken(14, 20, 0, 0, BuiltInType.INT64, true, 323),
      BadToken(21, 26, 0, 0)
    )

    for (i <- 0 to expected.length - 1) {
      if (expected(i).isInstanceOf[BadToken]) {
        val result = equalTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      } else {
        val result = equalIntegerLiteralTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      }
    }
  }

  test("Simple integer u") {
    val code = "1235 23434u32 323u64 123a23"
    val actual = l.lex(code)
    val expected = List[Token](
      IntegerLiteralToken(0, 4, 0, 1, BuiltInType.INT64, false, 1235),
      IntegerLiteralToken(5, 13, 0, 0, BuiltInType.UINT32, true, 23434),
      IntegerLiteralToken(14, 20, 0, 0, BuiltInType.UINT64, true, 323),
      BadToken(21, 26, 0, 0)
    )
    for (i <- 0 to expected.length - 1) {
      if (expected(i).isInstanceOf[BadToken]) {
        val result = equalTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      } else {
        val result = equalIntegerLiteralTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      }
    }
  }
}

class SimpleRuneTokenTest extends munit.FunSuite {
  test("Unicode Rune") {
    val code = """'u' 'r' 'as' '\n' '\U+12AD' '\U+23DAB'"""
    val actual = l.lex(code)
    val expected = List[Token](
      RuneLiteralToken(1, 1, 1, 1, 'u'),
      RuneLiteralToken(1, 1, 1, 1, 'r'),
      BadToken(1, 1, 1, 1),
      RuneLiteralToken(1, 1, 1, 1, '\n'),
      RuneLiteralToken(1, 1, 1, 1, '\u12ad'),
      RuneLiteralToken(1, 1, 1, 1, 146859)
    )
    for (i <- 0 to expected.length - 1) {
      if (expected(i).isInstanceOf[BadToken]) {
        val result = equalTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      } else {
        val result = equalRuneTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      }
    }
  }

  test("Simple rune") {
    val code = """'\v' '\a' '\\' '5' 'd'"""
    val actual = l.lex(code)
    val expected = List[Token](
      RuneLiteralToken(1, 1, 1, 1, 11),
      RuneLiteralToken(1, 1, 1, 1, 7),
      RuneLiteralToken(1, 1, 1, 1, '\\'),
      RuneLiteralToken(1, 1, 1, 1, '5'),
      RuneLiteralToken(1, 1, 1, 1, 'd')
    )
    for (i <- 0 to expected.length - 1) {
      if (expected(i).isInstanceOf[BadToken]) {
        val result = equalTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      } else {
        val result = equalRuneTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      }
    }
    val result = testCoverage(actual, code.length())
    assert(result(0), result(1))
  }

  test("Simple rune with \n") {
    val code = """'\v' '\a' '\\' '5' 'd'
'd'"""
    val actual = l.lex(code)
    val expected = List[Token](
      RuneLiteralToken(1, 1, 1, 1, 11),
      RuneLiteralToken(1, 1, 1, 1, 7),
      RuneLiteralToken(1, 1, 1, 1, '\\'),
      RuneLiteralToken(1, 1, 1, 1, '5'),
      RuneLiteralToken(1, 1, 1, 1, 'd'),
      RuneLiteralToken(1, 1, 1, 1, 'd')
    )
    for (i <- 0 to expected.length - 1) {
      if (expected(i).isInstanceOf[BadToken]) {
        val result = equalTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      } else {
        val result = equalRuneTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      }
    }
    val result = testCoverage(actual, code.length())
    assert(result(0), result(1))
  }
}

class SimpleStringTokenTest extends munit.FunSuite {
  test("Unicode string") {
    val code = """ "\U+12DA \n \r \" \U+32AD" """
    val actual = l.lex(code)
    val expected = List[Token](
      StringLiteralToken(1, 1, 1, 1, "\u12da \n \r \" \u32ad")
    )
    for (i <- 0 to expected.length - 1) {
      if (expected(i).isInstanceOf[BadToken]) {
        val result = equalTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      } else {
        val result = equalStringTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      }
    }
    val result = testCoverage(actual, code.length())
    assert(result(0), result(1))
  }

  test("Simple string") {
    val code = """ "ABds asdsad""""
    val actual = l.lex(code)
    val expected = List[Token](
      StringLiteralToken(1, 1, 1, 1, "ABds asdsad")
    )
    for (i <- 0 to expected.length - 1) {
      if (expected(i).isInstanceOf[BadToken]) {
        val result = equalTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      } else {
        val result = equalStringTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      }
    }
    val result = testCoverage(actual, code.length())
    assert(result(0), result(1))
  }
}

class SimpleBooleanTokenTest extends munit.FunSuite {
  test("Simple bool") {
    val code = """true false"""
    val actual = l.lex(code)
    val expected = List[Token](
      BooleanLiteralToken(0, 4, 0, 1, true),
      BooleanLiteralToken(5, 9, 0, 0, false)
    )
    for (i <- 0 to expected.length - 1) {
      val result = equalBooleanTokens(actual.get(i), expected(i))
      assert(result(0), result(1))
    }
    val result = testCoverage(actual, code.length())
    assert(result(0), result(1))
  }
}

class SimpleIndentTokenTest extends munit.FunSuite {
  test("Simple ident") {
    val code = """asd
  sd"""
    val actual = l.lex(code)
    val expected = List[Token](
      IdentifierToken(0, 3, 0, 1, "asd", null),
      IndentationToken(3, 3, 0, 0, 1),
      IdentifierToken(4, 7, 2, 0, "sd", null),
    )
    for (i <- 0 to expected.length - 1) {
      if (expected(i).isInstanceOf[IdentifierToken]) {
        val result = equalTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      } else {
        val result = equalIndentationTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      }
    }
    val result = testCoverage(actual, code.length())
    assert(result(0), result(1))
  }

  test("Simple ident") {
    val code = """asd
  sd"""
    val actual =  l.lex(code)
    val expected = List[Token](
      IdentifierToken(0, 3, 0, 1, "asd", null),
      IndentationToken(3, 3, 0, 0, 1),
      IdentifierToken(4, 7, 2, 0, "sd", null),
    )
    for (i <- 0 to expected.length - 1) {
      if (expected(i).isInstanceOf[IdentifierToken]) {
        val result = equalTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      } else {
        val result = equalIndentationTokens(actual.get(i), expected(i))
        assert(result(0), result(1))
      }
    }
    val result = testCoverage(actual, code.length())
    assert(result(0), result(1))
  }
}

class CoverageTest extends munit.FunSuite {
  test("Coverage 1") {
    val code = """    asd"""
    val actual = l.lex(code)

    val result = testCoverage(actual, code.length())
    assert(result(0), result(1))
  }

  test("Coverage 2") {
    val code = """asd  asd"""
    val actual = l.lex(code)

    val result = testCoverage(actual, code.length())
    assert(result(0), result(1))
  }

  test("Coverage 3") {
    val code = """asd () asd class object"""
    val actual = l.lex(code)

    val result = testCoverage(actual, code.length())
    assert(result(0), result(1))
  }

  test("Coverage 4") {
    val code = "\n  class Indent8\n    val x = 42"
    val actual = l.lex(code)

    val result = testCoverage(actual, code.length())
    assert(result(0), result(1))
  }

  test("Comment") {
    val code = "\n  class Indent8\n    val x = 42\n    # ASDsdffsa sad"
    val actual = l.lex(code)

    val result = testCoverage(actual, code.length())
    assert(result(0), result(1))
  }


}
