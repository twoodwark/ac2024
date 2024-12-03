import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map, HashMap}
import scala.util.matching.Regex.Match
object Day3 {
  def main(args: Array[String]): Unit = {
    val in = Source.stdin.mkString
    println(countMul2(in))
  }

  // part 1
  def countMul(in: String): Int = {
    val pat = raw"mul\(([0-9]+),([0-9]+)\)".r
    val adds =
      for (m <- pat.findAllMatchIn(in))
        yield m.group(1).toInt * m.group(2).toInt
    adds.sum
  }
  // part 2
  def countMul2(in: String): Int = {
    val pat = raw"do(n't)?\(\)|mul\(([0-9]+),([0-9]+)\)".r
    var enabled = true
    var summed = 0
    for (m <- pat.findAllMatchIn(in)) {
      if (m.group(2) != null)
        summed += (if (enabled) m.group(2).toInt * m.group(3).toInt else 0)
      else {
        enabled = m.group(1) == null
      }
    }
    summed
  }
}
