package ParserImplementation.Utils

import syspro.tm.parser.ErrorCode
case class MyErrorCode(var code: Int = -1, information: String = "") extends ErrorCode {
  override def name(): String = messages(code)
  
  // 20, 21, .. codes stands for errors that happened during semantic analysis
  val messages: Map[Int, String] = Map(
    -1 -> "Unknown Error",
    0 -> "TypeDefStartError",
    1 -> "No Identifier",
    2 -> "Missing Type Parametr Definition",
    3 -> "Missing Variable Definition Start",
    4 -> "Missing Name Expression After Colon",
    5 -> "Error in Generic Name Expression",
    6 -> "Missing Expression",
    7 -> "IDNENT ERROR",
    10 -> "Two Separated Symbols Cannot Follow Each Other",
    20 -> ("No such type declared in code " + information)
  )
}

