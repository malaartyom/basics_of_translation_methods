import scala.collection.mutable
import syspro.tm.lexer.Symbol

case object Symbols {
  private val shortSymbols = mutable.HashMap(
    "." -> Symbol.DOT,
    ":" -> Symbol.COLON,
    "," -> Symbol.COMMA,
    "+" -> Symbol.PLUS,
    "-" -> Symbol.MINUS,
    "*" -> Symbol.ASTERISK,
    "/" -> Symbol.SLASH,
    "%" -> Symbol.PERCENT,
    "!" -> Symbol.EXCLAMATION,
    "~" -> Symbol.TILDE,
    "&" -> Symbol.AMPERSAND,
    "|" -> Symbol.BAR,
    "^" -> Symbol.CARET,
    "<" -> Symbol.LESS_THAN,
    ">" -> Symbol.GREATER_THAN,
    "[" -> Symbol.OPEN_BRACKET,
    "]" -> Symbol.CLOSE_BRACKET,
    "(" -> Symbol.OPEN_PAREN,
    ")" -> Symbol.CLOSE_PAREN,
    "=" -> Symbol.EQUALS,
    "?" -> Symbol.QUESTION,
  )
  private val longSymbols = mutable.HashMap(
    "&&" -> Symbol.AMPERSAND_AMPERSAND,
    "||" -> Symbol.BAR_BAR,
    "<=" -> Symbol.LESS_THAN_EQUALS,
    ">=" -> Symbol.GREATER_THAN_EQUALS,
    "<<" -> Symbol.LESS_THAN_LESS_THAN,
    ">>" -> Symbol.GREATER_THAN_GREATER_THAN,
    "==" -> Symbol.EQUALS_EQUALS,
    "!=" -> Symbol.EXCLAMATION_EQUALS,
    "<:" -> Symbol.BOUND
  )

  def isSymbol(currentPoint: Char, nextPoint: Char): Boolean =
    longSymbols.contains(currentPoint.toString + nextPoint.toString) || shortSymbols.contains(currentPoint.toString)
}
