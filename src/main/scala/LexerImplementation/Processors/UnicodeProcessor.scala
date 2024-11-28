package LexerImplementation.Processors


import java.*
import java.text.Normalizer
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

case class UnicodeProcessor(str: String = "") {

  private val codePoints: List[Integer] = Normalizer.
    normalize(str, Normalizer.Form.NFC).codePoints().iterator().asScala.toList


  def get(idx: Int): String = {
    val i = idx
    if (i < codePoints.length) {
      val codePoint = codePoints(i)
      return Character.toChars(codePoint).mkString
    }
    null
  }

  def length: Int = codePoints.length

}
object UnicodeProcessor {
  def unescape(input: String): String = {
    val unicodePattern = """\\[uU]\+?([0-9a-fA-F]{4,8})""".r

    unicodePattern.replaceAllIn(input, m => {
      val codePoint = Integer.parseInt(m.group(1), 16)
      new String(Character.toChars(codePoint))
    })
  }

//  def main(args: Array[String]): Unit = {
//    val escapedString = "\\U+1D6A8\\U+00AD\\U+10341"
//    val unescapedString = unescape(escapedString)
//    println(s"Original: $escapedString")
//    println(s"Unescaped: $unescapedString")
//  }
}

