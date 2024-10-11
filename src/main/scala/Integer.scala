import scala.util.matching.Regex

case object Integer {
  val INTEGER: Regex = "[0-9]+(i32|i64|u32|u64)?".r
}
