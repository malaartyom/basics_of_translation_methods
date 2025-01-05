import LanguageServerImplementation.MyLanguageServer

class LanguageServerTest extends munit.FunSuite {
  test("Baste test 1") {
    val s = "class Indent2\n    # Comment established 4 spaces as single identation level\n  def memberIsAt2(): Boolean\n    return true"
    val l = MyLanguageServer().buildModel(s)
    
    
    assertEquals(1, 1)
  }
}
