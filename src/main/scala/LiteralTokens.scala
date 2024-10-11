import Boolean.BOOLEAN
import Integer.INTEGER
import Runes.RUNE
import StringLiteral.STRING

case object LiteralTokens {

  def isBoolean(s: String): Boolean = BOOLEAN.matches(s)

  def isInteger(s: String): Boolean = INTEGER.matches(s)

  def isRune(s: String): Boolean = RUNE.matches(s)

  def isString(s: String): Boolean = STRING.matches(s)

}
