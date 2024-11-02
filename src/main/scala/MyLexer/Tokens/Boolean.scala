package MyLexer.Tokens

import scala.util.matching.Regex

case object Boolean {
  val BOOLEAN: Regex = "true|false".r
}
