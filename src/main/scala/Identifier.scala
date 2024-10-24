import scala.util.matching.Regex

case object Identifier {
  val IDENTIFIER: Regex = """[\p{L}_][\p{L}\p{Nd}\p{Pc}\p{Mn}\p{Mc}\p{Cf}_]*""".r
}
