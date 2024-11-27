package LexerImplementation.Processors

import java.*
import java.text.Normalizer
import scala.jdk.CollectionConverters.*

case class UnicodeProcessor(str: String = "") {

  def evaluateEscapeSequence(input: String): String = {
    val result = input.replaceAllLiterally("\\b", "\b")
      .replaceAllLiterally("\\t", "\t")
      .replaceAllLiterally("\\n", "\n")
      .replaceAllLiterally("\\f", "\f")
      .replaceAllLiterally("\\r", "\r")
      .replaceAllLiterally("\\\"", "\"")
      .replaceAllLiterally("\\'", "\'")
      .replaceAllLiterally("\\\\", "\\")

    val unicodeRegex = "\\\\u([0-9a-fA-F]{4})".r
    val finalResult = unicodeRegex.replaceAllIn(result, m => {
      val codePoint = Integer.parseInt(m.group(1), 16)
      Character.toChars(codePoint).mkString
    })
    finalResult


  }

  private val codePoints: List[Integer] = Normalizer.
    normalize(evaluateEscapeSequence(str), Normalizer.Form.NFC).codePoints().iterator().asScala.toList


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
