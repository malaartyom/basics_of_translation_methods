import scala.collection.mutable
import syspro.tm.lexer.Keyword

case object Keywords {
  private val HardKeywords: mutable.HashMap[String, Keyword] = mutable.HashMap(
    "this" -> Keyword.THIS,
    "super" -> Keyword.SUPER,
    "is" -> Keyword.IS,
    "if" -> Keyword.IF,
    "else" -> Keyword.ELSE,
    "for" -> Keyword.FOR,
    "in" -> Keyword.IN,
    "while" -> Keyword.WHILE,
    "def" -> Keyword.DEF,
    "var" -> Keyword.VAR,
    "val" -> Keyword.VAL,
    "return" -> Keyword.RETURN,
    "break" -> Keyword.BREAK,
    "continue" -> Keyword.CONTINUE,
    "abstract" -> Keyword.ABSTRACT,
    "virtual" -> Keyword.VIRTUAL,
    "override" -> Keyword.OVERRIDE,
    "native" -> Keyword.NATIVE
  )

  private val SoftKeywords: mutable.HashMap[String, Keyword] = mutable.HashMap(
    "class" -> Keyword.CLASS,
    "interface" -> Keyword.INTERFACE,
    "object" -> Keyword.OBJECT,
    "null" -> Keyword.NULL
  )

  def isHardKeyword(s: String): Boolean = HardKeywords.contains(s)

  def isSoftKeyword(s: String): Boolean = SoftKeywords.contains(s)

  def isKeyword(s: String): Boolean = isSoftKeyword(s) || isHardKeyword(s)

  def getHardKeyword(s: String): Keyword = HardKeywords(s)

  def getSoftKeyword(s: String): Keyword = SoftKeywords(s)

}
