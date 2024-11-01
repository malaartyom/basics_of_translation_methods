trait Extractor {
  def extract(s: String, stop: String, idx: Int, function: (String, String) => Boolean = (x, y) => x != y): String = {
    val extractor = UnicodeProcessor(s)
    var i = idx
    var extracted: String = ""
    while (i < extractor.length && function(extractor.get(i), stop)) {
      extracted += extractor.get(i)
      i += 1
    }
    return extracted
  }
}
