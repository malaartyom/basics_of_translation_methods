import scala.collection.JavaConverters.seqAsJavaListConverter

import LexerImplementation.Tokenizer
import syspro.tm.lexer.*
import java.{util => ju}


def equalTokens(t1: Token, t2: Token): (Boolean, String) = {
  if (t1.toString() != t2.toString()) {
    return (false, s"Wrong toString: ${t1.toString()} <-> ${t2.toString()}")
  }
  return (true, null)
}

def equalIdentifierTokens(t1: Token, t2: Token): (Boolean, String) = {
  val it1 = t1.asInstanceOf[IdentifierToken]
  val it2 = t2.asInstanceOf[IdentifierToken]

  if (it1.value != it2.value) {
    return (false, s"Wrong value: ${it1.value} <-> ${it2.value}")
  } else if (it1.contextualKeyword != it1.contextualKeyword) {
    return (false, s"Wrong contextualKeyword: ${it1.contextualKeyword} <-> ${it2.contextualKeyword}")
  } else if (it1.toString() != it2.toString()) {
    return (false, s"Wrong toString: ${it1.toString()} <-> ${it2.toString()}")
  }
  return (true, null)
}

def equalKeywordTokens(t1: Token, t2: Token): (Boolean, String) = {
  val it1 = t1.asInstanceOf[KeywordToken]
  val it2 = t2.asInstanceOf[KeywordToken]

  if (it1.keyword != it1.keyword) {
    return (false, s"Wrong Keyword: ${it1.keyword} <-> ${it2.keyword}")
  } else if (it1.toString() != it2.toString()) {
    return (false, s"Wrong toString: ${it1.toString()} <-> ${it2.toString()}")
  }
  return (true, null)
}

def equalSymbolTokens(t1: Token, t2: Token): (Boolean, String) = {
  val it1 = t1.asInstanceOf[SymbolToken]
  val it2 = t2.asInstanceOf[SymbolToken]

  if (it1.symbol != it1.symbol) {
    return (false, s"Wrong symbol: ${it1.symbol} <-> ${it2.symbol}")
  } else if (it1.toString() != it2.toString()) {
    return (false, s"Wrong toString: ${it1.toString()} <-> ${it2.toString()}")
  }
  return (true, null)
}

def equalIntegerLiteralTokens(t1: Token, t2: Token): (Boolean, String) = {
  val it1 = t1.asInstanceOf[IntegerLiteralToken]
  val it2 = t2.asInstanceOf[IntegerLiteralToken]

  if (it1.value != it2.value) {
    return (false, s"Wrong value: ${it1.value} <-> ${it2.value}")
  }else if (it1.hasTypeSuffix != it2.hasTypeSuffix) {
    return (false, s"Wrong hasTypeSuffix: ${it1.hasTypeSuffix} <-> ${it2.hasTypeSuffix}")
  }else if (it1.`type`!= it2.`type`) {
    return (false, s"Wrong type: ${it1.`type`} <-> ${it2.`type`}")
  } else if (it1.toString() != it2.toString()) {
    return (false, s"Wrong toString: ${it1.toString()} <-> ${it2.toString()}")
  }
  return (true, null)
}

def equalRuneTokens(t1: Token, t2: Token): (Boolean, String) = {
  val it1 = t1.asInstanceOf[RuneLiteralToken]
  val it2 = t2.asInstanceOf[RuneLiteralToken]

  if (it1.value != it2.value) {
    return (false, s"Wrong value: ${it1.value} <-> ${it2.value}")
  } else if (it1.toString() != it2.toString()) {
    return (false, s"Wrong toString: ${it1.toString()} <-> ${it2.toString()}")
  }
  return (true, null)
}

def equalStringTokens(t1: Token, t2: Token): (Boolean, String) = {
  val it1 = t1.asInstanceOf[StringLiteralToken]
  val it2 = t2.asInstanceOf[StringLiteralToken]

  if (it1.value != it2.value) {
    return (false, s"Wrong value: ${it1.value} <-> ${it2.value}")
  } else if (it1.toString() != it2.toString()) {
    return (false, s"Wrong toString: ${it1.toString()} <-> ${it2.toString()}")
  }
  return (true, null)
}

def equalBooleanTokens(t1: Token, t2: Token): (Boolean, String) = {
  val it1 = t1.asInstanceOf[BooleanLiteralToken]
  val it2 = t2.asInstanceOf[BooleanLiteralToken]

  if (it1.value != it2.value) {
    return (false, s"Wrong value: ${it1.value} <-> ${it2.value}")
  } else if (it1.toString() != it2.toString()) {
    return (false, s"Wrong toString: ${it1.toString()} <-> ${it2.toString()}")
  }
  return (true, null)
}

def equalIndentationTokens(t1: Token, t2: Token): (Boolean, String) = {
  val it1 = t1.asInstanceOf[IndentationToken]
  val it2 = t2.asInstanceOf[IndentationToken]

  if (it1.difference != it2.difference) {
    return (false, s"Wrong value: ${it1.difference} <-> ${it2.difference}")
  } else if (it1.start != it2.start) {
    return (false, s"Wrong start: ${it1.start} <-> ${it2.start}")
  } else if (it1.end != it2.end) {
    return (false, s"Wrong end: ${it1.end} <-> ${it2.end}")
  } else if (it1.toString() != it2.toString()) {
    return (false, s"Wrong toString: ${it1.toString()} <-> ${it2.toString()}")
  }
  return (true, null)
}

def testCoverage(tokens: ju.List[Token], length: Int): (Boolean, String) = {
  var counter = 0
  for (i <- 0 to tokens.size() - 1) {
    if (!tokens.get(i).isInstanceOf[IndentationToken]) {
      if (tokens.get(i).start != counter) {
        return (false, s"Wrong counter on token $i: ${tokens.get(i).start} != $counter")
      }
      counter += tokens.get(i).end - tokens.get(i).start + 1
    }
  }
  if (length != counter) {
    return (false, s"Wrong all in all length")
  }
  return (true, null)

}


// def doTest(actual: ju.List[Token], expected: )
//     val triviaExpected = code.count(x => x == ' ')
//     var triviaCurrent = 0

//     var counter = 0

//     for (i <- 0 to expected.length - 1) {
//       val token = actual.get(i)
//       val result = equalSymbolTokens(token, expected(i))
//       assert(result(0), result(1))
//       triviaCurrent += token.leadingTriviaLength + token.trailingTriviaLength
//       assertEquals(token.start, counter, "Error Token: " + token.toString())
//       counter = token.end + 1
//     }
//     assertEquals(code.length(), counter)
//     assertEquals(triviaCurrent, triviaExpected)

