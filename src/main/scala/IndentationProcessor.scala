import Trivia.WHITESPACE

case class IndentationProcessor() {
  private val WHITESPACE: String = " "
  private val TABULATION: String = """\t"""

  private var currentIndentationLevel = 0
  private var currentIndentationLength = -1

  def dropLevel(): Int = {
    val tmp = currentIndentationLevel
    currentIndentationLevel = 0
    currentIndentationLength = -1
    return -tmp
  }

  def updateLevel(): Unit = {
    currentIndentationLevel = currentIndentationLevel
    currentIndentationLength = currentIndentationLength
  }
  def updateLevel(num: Int): Unit = {
    currentIndentationLevel = num
  }

  def updateLength(num: Int): Unit = {
    currentIndentationLength = num
  }

  def countIndentation(s: String): Int = {
    val indent: String = extractIndentation(s)
    if (indent.length % 2 != 0) {
      updateLevel()
      return 0
    }
    else if (currentIndentationLevel == 0 && indent.nonEmpty) {
      currentIndentationLength = indent.length
      currentIndentationLevel = 1
      return 1
    }
    else if (indent.length % currentIndentationLength != 0) {
      updateLevel()
      return 0
    }
    else if (indent.isEmpty) {
      val numOdDedent = dropLevel()
      return numOdDedent
    }
    else if (indent.length % currentIndentationLength == 0) {
      val N = indent.length / currentIndentationLength
      val prevIndentationLevel = currentIndentationLevel
      currentIndentationLevel = N
      currentIndentationLength = indent.length
      return N - prevIndentationLevel
    }
    return 0
  }

  def hasOnlyWhitespaces(s: String): Boolean = Trivia.WHITESPACE.matches(s)

  def hasIndentation(s: String): Boolean = s.startsWith(WHITESPACE) || s.startsWith(TABULATION)

  def isEndOfFile(all_file: String, next_line: String, current_index: Int): Boolean = (current_index + next_line.length >= all_file.length)

  private def extractIndentation(s: String): String = {
    var indentation: String = ""
    var i = 0
    while (s(i) == ' ') {
      indentation += s(i)
      i += 1
    }
    return indentation
  }

}
