package LanguageServerImplementation

import LanguageServerImplementation.Context.{GenericEnvironment, TypeEnvironment}
import syspro.tm.symbols.{TypeParameterSymbol, TypeSymbol}

import scala.annotation.targetName
import scala.collection.mutable
object Context {
  type TypeEnvironment = scala.collection.mutable.HashMap[String, TypeSymbol]
  type GenericEnvironment = scala.collection.mutable.HashMap[String, TypeParameterSymbol]
}

class Context(
               var genericContexts: mutable.Stack[GenericEnvironment] = mutable.Stack(),
               var types: TypeEnvironment = mutable.HashMap()
             )
{

  types = types ++ mutable.HashMap(
    "Boolean" -> BaseTypeSymbol("Boolean"),
    "Int32" -> BaseTypeSymbol("Int32"),
    "Int64" -> BaseTypeSymbol("Int64"),
    "UInt32" -> BaseTypeSymbol("UInt32"),
    "UInt64" -> BaseTypeSymbol("UInt64"),
    "Rune" -> BaseTypeSymbol("Rune"),
    "Iterable" -> null,
    "PrimitiveIntrinsics" -> null,
    "System" -> null,
    "Iterator" -> null,
    "Iterable" -> null,
    "RangeIterator" -> null,
    "Range" -> null,
    "ArrayIterator" -> null,
    "Array" -> null,
    "ArrayListIterator" -> null,
    "ArrayList" -> null,
    "String" -> null
  )


  def setEnvironment(key: String): Unit =
    if (types(key) != null)
      types(key).asInstanceOf[MyTypeSymbol].setEnvironment(types)

  def push(key: String = null, value: TypeParameterSymbol = null): Unit =
    if (key == null && value == null) genericContexts.push(mutable.HashMap())
    genericContexts.push(genericContexts.pop() += (key -> value))

  def pop(): GenericEnvironment = genericContexts.pop()

  def getClass(key: String): TypeSymbol = types(key)

  def getParameter(key: String): TypeParameterSymbol = genericContexts.top(key)

  def containsClass(key: String): Boolean = types.contains(key)

  def containsParam(key: String): Boolean =
    if (genericContexts.isEmpty) return false
    genericContexts.top.contains(key)

  def containsEmpty(key: String): Boolean = types.contains(key) && types(key).isInstanceOf[EmptyTypeSymbol]

  def add(key: String, value: TypeSymbol): Unit = types += (key -> value)

  def update(key: String, value: TypeSymbol | TypeParameterSymbol): Unit =
    value match
      case tp: TypeSymbol => types(key) = tp
      case tps: TypeParameterSymbol => push(key, tps)

  def updateTypes(types: TypeEnvironment): Unit = this.types = this.types ++ types

  def updateParam(generics: GenericEnvironment): Unit = genericContexts.push(genericContexts.pop() ++ generics)

  @targetName("+=")
  def +=(thatCtx: Context): Unit =
    updateTypes(thatCtx.types)
    updateParam(thatCtx.pop())

}
