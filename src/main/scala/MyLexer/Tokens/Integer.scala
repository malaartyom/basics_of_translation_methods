package MyLexer.Tokens

import scala.util.matching.Regex

case object Integer {
  val INT_SUFFIX: Regex = "i32|i64|u32|u64".r
  val INTEGER: Regex = "[0-9]+(i32|i64|u32|u64)?".r
}
