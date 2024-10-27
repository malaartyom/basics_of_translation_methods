import Boolean.BOOLEAN
import Integer.INTEGER
import Integer.INT_SUFFIX
import Runes.{RUNE, RUNE_CHAR, RUNE_CHARACTER, RUNE_INTERIOR}
import StringLiteral.{STRING, STRING_INTERIOR, STRING_START}
import syspro.tm.lexer.BuiltInType

case object LiteralTokens {

  def isBoolean(s: String): Boolean = BOOLEAN.matches(s)

  def isInteger(s: String): Boolean = INTEGER.matches(s)

  def isStringInterior(s: String): Boolean = STRING_INTERIOR.matches(s)

  def isRuneInterior(s: String): Boolean = RUNE_INTERIOR.matches(s)

  def isSuffix(s: String): Boolean = INT_SUFFIX.matches(s)

  def hasSuffix(s: String): Boolean = INT_SUFFIX.matches(s.slice(s.length - 3, s.length))

  def isStringStart(s: String): Boolean = STRING_START.matches(s)

  def isRuneStart(s: String): Boolean = RUNE_CHAR.matches(s)

  def getSuffix(s: String, hasSuffix: Boolean): BuiltInType = {
    if (!hasSuffix) return BuiltInType.INT64
    val suffix = s.slice(s.length - 3, s.length)
    suffix match {
      case "i32" => BuiltInType.INT32
      case "i64" => BuiltInType.INT64
      case "u32" => BuiltInType.UINT32
      case "u64" => BuiltInType.UINT64
    }
  }

  def getInt(s: String, hasSuffix: Boolean): Long =
    if (!hasSuffix) {
      return s.toLong
    }
    s.slice(0, s.length - 3).toLong

  def getBoolean(s: String): Boolean =
    s match {
      case "false" => false
      case "true" => true
    }
}
