import java.text.Normalizer
import java.util.PrimitiveIterator

case class UnicodeProcessor(str: String = "") {

  private val codePoints: PrimitiveIterator.OfInt = Normalizer.normalize(str, Normalizer.Form.NFC).codePoints().iterator()
  val len: Long = Normalizer.normalize(str, Normalizer.Form.NFC).codePoints().count()
  
  
  def get(i: Int): String = {
    var str: String = ""
    var idx = i
    while (idx > 0 && codePoints.hasNext) {
      val codePoint = codePoints.nextInt()
      idx -= 1
      str = Character.toChars(codePoint).mkString
    }
    return str
  }
  def get(): String = {
    
    if (codePoints.hasNext) {
      val codePoint = codePoints.nextInt()
      return Character.toChars(codePoint).mkString
    } 
    return null
  }
  
  def hasNext(): Boolean = codePoints.hasNext
}
