import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.View.Iterate
/*
 */
case class Stone(value: Long) {
  def blink =
    val numdigits = Math.log10(value.toDouble).toLong + 1
    value match {
      case 0 => List(Stone(1))
      case v if (numdigits % 2 == 0) => {
        val div = Math.pow(10, numdigits.toDouble / 2).toLong
        val left = Math.floorDiv(v, div)
        val right = v % div
        List(Stone(left), Stone(right))
      }
      case _ => List(Stone(value * 2024))
    }
}
object Day11 extends App {
  val stones = parseStones
  part1().pipe(println)

  def part1(blinks: Int = 25) =
    (0 to blinks - 1)
      .foldLeft(stones) { (s, _) =>
        s.flatMap(_.blink)
      }
      .length

  def parseStones =
    Source.stdin.mkString.strip().split(" ").toList.map(_.toInt).map(Stone(_))
}
