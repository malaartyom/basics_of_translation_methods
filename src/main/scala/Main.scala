import Trivia.WHITESPACE
import StringLiteral.STRING

import scala.util.matching.Regex

object Main {
  def main(args: Array[String]): Unit = {
    var lexer = Tokenizer()
//    println(WHITESPACE.matches(" "))
//    println(Runes.RUNE.matches("""'\n'"""))
//
//    println(lexer.lex("""var x = 'b'  '\n'  var govno = "hui"   """))
//    println(lexer.lex(
//      """var xyz=123u32+124u32   #  bro
//        |class Object
//        |
//        |""".stripMargin))
    var s = """class Object
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

    println(s.length)
    println(s.slice(840, 890))
    println(lexer.lex(s))
    //    println(STRING.matches(""""Hifjn wuhuhwuf jjuh wuhubbfuhwuhfuwbfubfqn26378e9uf9jedndsa$T bro""""))
    val INTEGER: Regex = "[0-9]+(i32|i64|u32|u64)?".r
  }
}

