import Trivia.WHITESPACE


case class IndentationProcessor() extends Extractor {
  
  var lastLineBreak = 0
  private var currentIndentationLevel = 0
  private var currentIndentationLength = -1
  
  def getCurrentIndentationLevel: Int = currentIndentationLevel
  
  def dropLevel(): Int = {
    val tmp = currentIndentationLevel
    currentIndentationLevel = 0
    -tmp
  }

  def updateLevel(): Unit = {
    currentIndentationLevel = currentIndentationLevel
    currentIndentationLength = currentIndentationLength
  }
  def countIndentation(s: String): Int = {
    val indent: String = IndentationProcessor.extract(s)
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
      return currentIndentationLevel - prevIndentationLevel
    }
    0
  }
  
}

object IndentationProcessor extends Extractor:

  private val WHITESPACE: String = " "
  private val TABULATION: String = """\t"""

  override def extract(s: String, stop: String = " ", idx: Int = 0, function: (String, String) => Boolean): String =
    super.extract(s, " ", 0, (x, y) => x == y)

  def hasOnlyWhitespaces (s: String): Boolean = Trivia.WHITESPACE.matches(s)

  def hasIndentation(s: String): Boolean = s.startsWith(WHITESPACE) || s.startsWith(TABULATION)

  def hasComment(s: String): Boolean = {
    val i = extract(s).length
    s(i) == '#' 
  }


end IndentationProcessor
