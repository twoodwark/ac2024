import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map, HashMap}
import scala.util.matching.Regex.Match
object Day3 {
  def main(args: Array[String]): Unit = {
    val in = Source.stdin.mkString
    println(countMul(in))
  }

  // part 1
  def countMul(in: String): Int = {
    val pat = raw"mul\(([0-9]+),([0-9]+)\)".r
    val adds =
      for (m <- pat.findAllMatchIn(in))
        yield m.group(1).toInt * m.group(2).toInt
    adds.sum
  }
}
