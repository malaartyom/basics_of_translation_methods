import java.*
import java.text.Normalizer
import java.util.PrimitiveIterator
import scala.jdk.CollectionConverters._

case class UnicodeProcessor(str: String = "") {

  val codePoints: List[Integer] = Normalizer.
    normalize(str, Normalizer.Form.NFC).codePoints().iterator().asScala.toList


  def get(idx: Int): String = {
    var i = idx
    if (i < codePoints.length) {
      val codePoint = codePoints(i)
      return Character.toChars(codePoint).mkString
    }
    return null
  }

  def last(): String = this.get(this.length - 1)

  def length: Int = codePoints.length

}
