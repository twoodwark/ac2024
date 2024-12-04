import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map, HashMap}
import scala.util.matching.Regex.Match
import scala.collection.immutable.Vector
import scala.util.ChainingOps._

/*
XXMAS
MSMSS
AXXAX
SAMXS
 */
object Day4 {
  def main(args: Array[String]): Unit = {
    val in = Source.stdin.mkString
    val parsed = parseMatrix(in)
    println(count1(parsed))
  }

  def parseMatrix(in: String): Vector[Vector[Char]] = {
    val vecs = in.linesIterator.map((l) => l.toCharArray.toVector)
    Vector.from(vecs)
  }
  // part 1
  def count1(m: Vector[Vector[Char]]) = {
    val width = m(0).length
    val height = m.length
    def cols =
      for (i <- 0 to width - 1) yield m.map(_(i))

      // Ugly
    def diag1 = {
      for (j <- 1 - width to height - 1) yield {
        // start on row j (=>0) at left
        val rowStart = j.max(0)
        val colStart = if (j < 0) (-j) else 0
        // continue diagonally as far as we can
        val maxoffset = (height - rowStart).min(width - colStart) - 1
        (0 to maxoffset).map((offs) => m(rowStart + offs)(colStart + offs))
      }
    }
    def diag2 = {
      for (j <- 1 - width to height - 1) yield {
        val rowStart = j.max(0)
        val colStart = if (j < 0) (-j) else 0
        val maxoffset = (height - rowStart).min(width - colStart) - 1
        (0 to maxoffset).map((offs) =>
          m(rowStart + offs)(width - 1 - colStart - offs)
        )
      }
    }
    // Chain rows, reverse rows, columns, reverse columns....
    def allAxesStrings: Iterable[String] = {
      Iterable.concat(m, cols, diag1, diag2).map(_.mkString)
    }
    // allAxesStrings
    //   .map(println)
    // hmm could be SIMD? Who cares
    val pat = "XMAS".r
    {
      for (s <- allAxesStrings)
        yield pat.findAllIn(s).length + pat.findAllIn(s.reverse).length
    }.sum
  }
}
