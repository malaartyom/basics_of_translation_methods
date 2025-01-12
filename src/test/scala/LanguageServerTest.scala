import LanguageServerImplementation.Symbols.{MyTypeParameterSymbol, MyTypeSymbol, MyVariableSymbol}
import LanguageServerImplementation.{MyLanguageServer, MySemanticModel}
import LexerImplementation.Tokenizer
import ParserImplementation.Parsing
import ParserImplementation.Parsing.{MyParseResult, MyParser}
import syspro.tm.parser.SyntaxKind
import syspro.tm.symbols.{SymbolKind, TypeSymbol}

import java.util
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

class LanguageServerTest extends munit.FunSuite {


  test("variable def") {
    val s = "val s: Int64 = 12"
    val l = Tokenizer().lex(s)
    val p = MyParser()
    p.setTokens(l.asScala.toVector)
    val parsingResult = p.matchStatement()
    val model = MySemanticModel(MyParseResult(null, null), null)
    val res = model.getLocal(parsingResult, null).map(x => x.asInstanceOf[MyVariableSymbol])
    assertEquals(res.head.name, "s")
    assertEquals(res.head.kind, SymbolKind.LOCAL)
    assertEquals(res.head.owner, null)
    assertEquals(res.head.`type`, model.lookupType("Int64"))
  }


  test("for") {
    val s =
      """for i in array
        |  val x: Int64 = 12
        |""".stripMargin
    val l = Tokenizer().lex(s)
    val p = MyParser()
    p.setTokens(l.asScala.toVector)
    val parsingResult = p.matchStatement()
    val model = MySemanticModel(MyParseResult(null, null), null)
    val res = model.getLocal(parsingResult, null).map(x => x.asInstanceOf[MyVariableSymbol])
    assertEquals(res.head.name, "i")
    assertEquals(res.head.kind, SymbolKind.LOCAL)
    assertEquals(res.head.owner, null)
//    assertEquals(res.head.definition, parsingResult.slot(1))
    assertEquals(res.head.`type`, null)


    assertEquals(res(1).name, "x")
    assertEquals(res(1).kind, SymbolKind.LOCAL)
    assertEquals(res(1).owner, null)
//    assertEquals(res(1).definition, parsingResult.slot(5).slot(0))
    assertEquals(res(1).`type`, model.lookupType("Int64"))
  }

  test("is_expression") {

    val s =
      """(2 + 2) is Int64 x""".stripMargin
    val l = Tokenizer().lex(s)
    val p = MyParser()
    p.setTokens(l.asScala.toVector)
    val parsingResult = p.matchStatement()
    val model = MySemanticModel(MyParseResult(null, null), null)
    val res = model.getLocal(parsingResult, null).map(_.asInstanceOf[MyVariableSymbol])
    assertEquals(res.head.name, "x")
    assertEquals(res.head.kind, SymbolKind.LOCAL)
    assertEquals(res.head.owner, null)
    assertEquals(res.head.definition, parsingResult.slot(0).slot(3))
    assertEquals(res.head.`type`, model.lookupType("Boolean"))

  }
  test("Clash 1") {
    val s =
      """interface Clash1<T>
        |  def apply(arg: T): Boolean
        |
        |class A <: Clash1<UInt64>
        |  def apply(arg: UInt64): Boolean
        |    return true
        |
        |class B <: Clash1<Int64>
        |  abstract def apply(arg: Int64): Boolean
        |    return true
        |""".stripMargin
    val server = MyLanguageServer()
    val model = server.buildModel(s)
    val types = model.typeDefinitions().asScala.map(_.asInstanceOf[MyTypeSymbol])


  }
  test("Base test 1") {
    val s = """class Indent2
              |    # Comment established 4 spaces as single identation level
              |  def memberIsAt2(): Boolean
              |    return true""".stripMargin
    val l = MyLanguageServer().buildModel(s)
    val types = l.typeDefinitions()

//    assertEquals(types.get(0).asInstanceOf[MyTypeSymbol],
//      MyTypeSymbol(typeArgs = ListBuffer.empty,
//        memberSymbols = ???,
//        baseTypesBuffer = ListBuffer.empty,
//        kind = SymbolKind.CLASS,
//        name = "Indent2",
//        definition = ,
//        isAbstract = false))

    
    assertEquals(1, 1)
  }

  test("Base test 2") {


    val s =
      """class Parent3<T <: Int64> <: X
        |  var y: X
        |  def bro(x: T): X
        |    println("bro")
        |    return x
        |interface X <: Parent3<Int> & Int64
        |  def pass()
        |    return 4""".stripMargin
    val model = MyLanguageServer().buildModel(s)
    val types = model.typeDefinitions().asScala.map(_.asInstanceOf[MyTypeSymbol])
    val parent3 = types.head
    assertEquals(types.head.name, "Parent3")
    val parent3BaseTypes = types.head.baseTypesBuffer.map(_.asInstanceOf[MyTypeSymbol])
    assertEquals(parent3BaseTypes.head.name, "X")
    assertEquals(parent3BaseTypes.head.isAbstract, true)
    assertEquals(parent3BaseTypes.head.kind, SymbolKind.INTERFACE)
    assertEquals(parent3BaseTypes.head.definition, model.root().slot(0).slot(1))
    assertEquals(parent3BaseTypes.head.typeArgs, ListBuffer.empty)

    assertEquals(parent3.memberSymbols.head.asInstanceOf[MyVariableSymbol].`type`, parent3BaseTypes.head)
    assertEquals(parent3.memberSymbols.head.asInstanceOf[MyVariableSymbol].kind, SymbolKind.FIELD)
    assertEquals(parent3.memberSymbols.head.asInstanceOf[MyVariableSymbol].owner, parent3)
    assertEquals(parent3.memberSymbols.head.asInstanceOf[MyVariableSymbol].name, "y")


    assertEquals(1, 1)
  }

  test("Generics") {

    val s =
      """class Parent3<T <: Int64> <: X
        |  var y: X
        |  def f(x: T): X
        |    println("f")
        |    return x
        |interface X <: Parent3<Int32> & Int64
        |  def pass()
        |    return 4
        |
        |class Y<F <: Parent3<Int64> >
        |   def x()
        |     return 2""".stripMargin
    val model = MyLanguageServer().buildModel(s).asInstanceOf[MySemanticModel]
    val types = model.typeDefinitions().asScala.map(_.asInstanceOf[MyTypeSymbol])
    val parent3 = types.head
    val Y = types(2)
    assertEquals(types.head.name, "Parent3")
    val parent3BaseTypes = types.head.baseTypesBuffer.map(_.asInstanceOf[MyTypeSymbol])
    assertEquals(parent3BaseTypes.head.name, "X")
    assertEquals(parent3BaseTypes.head.isAbstract, true)
    assertEquals(parent3BaseTypes.head.kind, SymbolKind.INTERFACE)
    assertEquals(parent3BaseTypes.head.definition, model.root().slot(0).slot(1))
    assertEquals(parent3BaseTypes.head.typeArgs, ListBuffer.empty)


    assertEquals(parent3.memberSymbols.head.asInstanceOf[MyVariableSymbol].`type`, parent3BaseTypes.head)
    assertEquals(parent3.memberSymbols.head.asInstanceOf[MyVariableSymbol].kind, SymbolKind.FIELD)
    assertEquals(parent3.memberSymbols.head.asInstanceOf[MyVariableSymbol].owner, parent3)
    assertEquals(parent3.memberSymbols.head.asInstanceOf[MyVariableSymbol].name, "y")


    assertEquals(Y.kind, SymbolKind.CLASS)
    

  }


  test("Standart Library Test") {

    val s =
      """class Object
        |object PrimitiveIntrinsics<T>
        |    native def default(): T # Available for all primitive or nullable types
        |    # The following are valid only for numeric T
        |    native def one(): T
        |    native def add(left: T, right: T): T
        |    native def subtract(left: T, right: T): T
        |    native def multiply(left: T, right: T): T
        |    native def divide(left: T, right: T): T
        |    native def remainder(left: T, right: T): T
        |    native def less(left: T, right: T): T
        |    native def greater(left: T, right: T): T
        |    native def toString(num: T): String
        |object System
        |    native def print(message: String)
        |    native def println(message: String)
        |    native def failFast(message: String)
        |interface Iterator<T>
        |    def hasNext(): Boolean
        |    def next(): T
        |interface Iterable<T>
        |    def iterator(): Iterator<T>
        |class RangeIterator<T> <: Iterator<T> # Supports only numeric T, no way to express in SysPro
        |    val _range: Range<T>
        |    val _next: T
        |    def this(range: Range<T>)
        |        this._range = range
        |        this._next = range.start
        |    def hasNext(): Boolean
        |        if PrimitiveIntrinsics<T>.greater(_range.step, PrimitiveIntrinsics<T>.default())
        |            return PrimitiveIntrinsics<T>.less(_next, _range.end)
        |        else
        |            return PrimitiveIntrinsics<T>.greater(_next, _range.end)
        |    def next(): T
        |        if !hasNext()
        |            System.failFast("No next element is available in Range<T>")
        |        val result = _next
        |        _next = PrimitiveIntrinsics<T>.add(_next, _range.step)
        |        return result
        |class Range<T> <: Iterable<T>
        |    val start: T
        |    val end: T
        |    val step: T
        |    def this(start: T, end: T, step: T)
        |        this.start = start
        |        this.end = end
        |        this.step = step
        |    def this(start: T, end: T)
        |        this.start = start
        |        this.end = end
        |        this.step = PrimitiveIntrinsics<T>.one()
        |    def this(end: T)
        |        this.start = PrimitiveIntrinsics<T>.default()
        |        this.end = end
        |        this.step = PrimitiveIntrinsics<T>.one()
        |    def iterator(): Iterator<T>
        |        return RangeIterator<T>(this)
        |class ArrayIterator<T> <: Iterator<T>
        |    val _array: Array<T>
        |    var _index: UInt64 = 0
        |    def this(array: Array<T>)
        |        this._array = array
        |    def hasNext(): Boolean
        |        return _index < _array.length
        |    def next(): T
        |        val result = _array[_index] # Will failFast if necessary
        |        _index = _index + 1u64
        |        return result
        |class Array<T> <: Iterable<T>
        |    native def this(length: UInt64, fill: T)
        |    native def length(): UInt64
        |    native def subscript(index: UInt64): T
        |    native def subscript(index: UInt64, value: T)
        |    def clone(newLength: UInt64, fill: T): Array<T>
        |        val result = Array<T>(newLength, fill)
        |        var copy = length
        |        if newLength < copy
        |            copy = newLength
        |        for i in RangeUInt64(copy)
        |            result[i] = this[i]
        |        return result
        |    def iterator(): Iterator<T>
        |        return ArrayIterator<T>(this)
        |class String
        |    val _runes: Array<Rune>
        |    def this(runes: Array<Rune>)
        |        this._runes = runes
        |    def subscript(index: UInt64): Rune
        |        return _runes[index]
        |class ArrayListIterator<T>
        |    val _list: ArrayList<T>
        |    var _index: UInt64 = 0
        |    def this(list: ArrayList<T>)
        |        this._list = list
        |    def hasNext(): Boolean
        |        return _index < _list.length
        |    def next(): T
        |        if !hasNext()
        |            System.failFast("No next element is available in ArrayList<T>")
        |        val result = _list[_index] # Will failFast if necessary
        |        _index = _index + 1u64
        |        return result
        |class ArrayList<T> <: Iterable<T>
        |    var _data = Array<T>(10, PrimitiveIntrinsics<T>.default())
        |    var _size: UInt64 = 0
        |    def length(): UInt64
        |        return _size
        |    def add(item: T)
        |        if _size == _data.length
        |            _data = _data.clone(_size * 2, PrimitiveIntrinsics<T>.default())
        |        _data[_size] = item
        |        _size = _size + 1u64
        |    def subscript(index: UInt64): T
        |        if index >= _size
        |            System.failFast("List element read out of bounds")
        |        return _data[index]
        |    def subscript(index: UInt64, value: T)
        |        if index >= _size
        |            System.failFast("List element write out of bounds")
        |        _data[index] = value
        |    def iterator(): Iterator<T>
        |        return ArrayListIterator<T>(this)
        |    def toArray(): Array<T>
        |        return _data.clone(_size, PrimitiveIntrinsics<T>.default())""".stripMargin
    val l = MyLanguageServer().buildModel(s)
    val types = l.typeDefinitions().asScala



    types

  }

  test("String") {
    val s =
      """class Parent3<T <: Int64> <: X
        |  var y: X = "ABCD"
        |  def bro(x: T): String
        |    println("bro")
        |    return x
        |interface X <: Parent3<Int> & String
        |  def pass()
        |    return 4""".stripMargin
    val model = MyLanguageServer().buildModel(s)
    val types = model.typeDefinitions().asScala.map(_.asInstanceOf[MyTypeSymbol])
    val parent3 = types.head
    assertEquals(types.head.name, "Parent3")
    val parent3BaseTypes = types.head.baseTypesBuffer.map(_.asInstanceOf[MyTypeSymbol])
    assertEquals(parent3BaseTypes.head.name, "X")
    assertEquals(parent3BaseTypes.head.isAbstract, true)
    assertEquals(parent3BaseTypes.head.kind, SymbolKind.INTERFACE)
    assertEquals(parent3BaseTypes.head.definition, model.root().slot(0).slot(1))
    assertEquals(parent3BaseTypes.head.typeArgs, ListBuffer.empty)

    assertEquals(parent3.memberSymbols.head.asInstanceOf[MyVariableSymbol].`type`, parent3BaseTypes.head)
    assertEquals(parent3.memberSymbols.head.asInstanceOf[MyVariableSymbol].kind, SymbolKind.FIELD)
    assertEquals(parent3.memberSymbols.head.asInstanceOf[MyVariableSymbol].owner, parent3)
    assertEquals(parent3.memberSymbols.head.asInstanceOf[MyVariableSymbol].name, "y")


    println(model.diagnostics())
  }

  test("getDefinition 1") {
//    val code = "val x = 3"
//    val t = Tokenizer()
//    val tokens = t.lex(code)
//    val p =  Parsing.MyParser();
//    p.setTokens(tokens.asScala.toVector)
//    val result = p.matchDefinition()
//
//    val model = MySemanticModel(null).getDefinition(result, null)

  }
}


class ComplicatedTest extends munit.FunSuite {

  test("My") {
    val code =
      """interface Iterator<T>
        |    def hasNext(): Boolean
        |    def next(): T
        |interface Iterable
        |    def iterator(i: String)""".stripMargin
    val result = MyLanguageServer().buildModel(code)
    println(result.typeDefinitions())
  }

  test("Parametrize undefined") {
    val code =
      """interface Iterator<T>
        |    def hasNext(): String<T>
        |    def next(): T
        |class A <: Iterator<T>
        |    val i: String<T>
        |    def hasNext(): String<T>""".stripMargin
    val result = MyLanguageServer().buildModel(code)
    println(result.typeDefinitions())
  }

  test("A lot parameters") {
    val code =
      """interface Iterator<T>
        |    def hasNext(): String<T>
        |    def next(): T
        |class A<T> <: Iterator<T>
        |    val i: Iterator<T>
        |    def hasNext(): String<T>""".stripMargin
    val result = MyLanguageServer().buildModel(code)
    println(result.typeDefinitions())
  }

  test("Type bounds") {
    val code =
      """class B<T>
        |class F<T>
        |interface Iterator<T>
        |    def hasNext(): String<T>
        |    def next(): T
        |class A<D, T <: B<T> & F<T>>
        |    val i: Iterator<String<T>>
        |    def hasNext(): String<T>""".stripMargin
    val result = MyLanguageServer().buildModel(code)
    println(result.typeDefinitions())
    result.diagnostics().forEach(d => println(d.errorCode()))
    result.lookupType("A").typeArguments().forEach(a => println(a.asInstanceOf[MyTypeParameterSymbol].bounds))
  }

  test("Type bounds undefined") {
    val code =
      """class A<T>
        |    val i: Iterator<String<T>>""".stripMargin
    val result = MyLanguageServer().buildModel(code)
    println(result.typeDefinitions())
    result.diagnostics().forEach(d => println(d.errorCode()))
  }

  test("Type bounds with Interface") {
    val code =
      """interface B<T>
        |object F<T>
        |interface Iterator<T>
        |    def hasNext(): String<T>
        |    def next(): T
        |class A<D, T <: B<T> & F<T>>
        |    val i: Iterator<String<T>>
        |    def hasNext(): String<T>""".stripMargin
    val result = MyLanguageServer().buildModel(code)
    println(result.typeDefinitions())
    result.diagnostics().forEach(d => println(d.errorCode()))
    result.lookupType("A").typeArguments().forEach(a => println(a.asInstanceOf[MyTypeParameterSymbol].bounds))
  }

}
