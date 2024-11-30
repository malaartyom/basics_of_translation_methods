package ParserImplementation
import syspro.tm.parser.ErrorCode
case class MyErrorCode(var code: Int = -1) extends ErrorCode {
  override def name(): String = messages(code)
  val messages: Map[Int, String] = Map(
    -1 -> "Unknown Error",
    0 -> "TypeDefStartError",
    1 -> "No Identifier",
    2 -> "Missing Type Parametr Definition",
    3 -> "Missing Variable Definition Start",
    4 -> "Missing Name Expression After Colon",
    5 -> "Error in Generic Name Expression",
    6 -> "Missing Expression",
    7 -> "IDNENT ERROR"
  )
}

