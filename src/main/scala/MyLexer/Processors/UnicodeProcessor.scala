package MyLexer.Processors

import java.*
import java.text.Normalizer
import java.util.PrimitiveIterator
import scala.jdk.CollectionConverters.*

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

  def last(): String = this.get(this.length - 1)

  def length: Int = codePoints.length

}
