package LexerImplementation.Tokens

import scala.util.matching.Regex

case object Identifier {
  val IDENTIFIER: Regex =
    """(?x)
    ^(
        [\p{L}\p{Nl}_]           
        [\p{L}\p{Nl}\p{Nd}\p{Pc}\p{Mn}\p{Mc}\p{Cf}]* 
    )$
""".r
}
