import Trivia.WHITESPACE

case object IndentationProcessor {
  private val WHITESPACE: String = " "
  private val TABULATION: String = """\t"""

  private var currentIndentationLevel = 0
  private var currentIndentationLength = -1

  def dropLevel(): Unit = {
    currentIndentationLevel = 0
  }

  def updateLevel(num: Int): Unit = {
    currentIndentationLevel = num
  }

  def updateLength(num: Int): Unit = {
    currentIndentationLength = num
  }

  def countIndentation(s: String): Unit = {
    var indent: String = extractIndentation(s)
    if (indent.length % 2 != 0) {
      ???
    }
    else if (currentIndentationLevel == 0) {
      currentIndentationLength += indent.length
      currentIndentationLevel += 1
    }
    else if (indent.length % currentIndentationLength != 0) {
      ???
    }
    else if (indent.isEmpty) {
      dropLevel()
    }
    // TODO: Если кратно
  }

  def hasOnlyWhitespaces(s: String): Boolean = Trivia.WHITESPACE.matches(s)

  def hasIndentation(s: String): Boolean = s.startsWith(WHITESPACE) || s.startsWith(TABULATION)

  def isEndOfFile(all_file: String, next_line: String, current_index: Int): Boolean = (current_index + next_line.length >= all_file.length)

  def extractIndentation(s: String): String = {
    var indentation= ""
    i = 0
    while (s(i) == ' ') {
      indentation += s(i)
      i += 1
    }
    return indentation
  }

}
