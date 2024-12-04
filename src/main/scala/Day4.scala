import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map, HashMap}
import scala.util.matching.Regex.Match
import scala.collection.immutable.Vector

/*
XXMAS
MSMSS
AXXAX
SAMXS


MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX


 */
object Day4 {
  def main(args: Array[String]): Unit = {
    val in = Source.stdin.mkString
    println(count2(in))
  }

  // part 2
  def count2(in: String) = {
    // HACK
    // compile a regex that will match X-MAS across rows
    // eg in a width-5 matrix, X-MAS pattern is 'M.S(.|\n){2}.A.(.|\n){2}M.S'
    // here dots may not match newline
    val width = in.linesIterator.next.length
    val distBetween = width - 3 + 1 // 3 for pattern width, 1 for the newline
    val pat = // needs lookahead to find overlapping matches!
      s"""(M|S)(?=.(M|S)(?:.|\\n){$distBetween}.A.(?:.|\\n){$distBetween}(M|S).(M|S))""".r
    println(pat)
    // check captures are ok
    pat
      .findAllMatchIn(in)
      .count((x) => (x.group(1) != x.group(4) && x.group(2) != x.group(3)))

  }

  def parseMatrix(in: String): Vector[Vector[Char]] = {
    val vecs = in.linesIterator.map((l) => l.toCharArray.toVector)
    Vector.from(vecs)
  }
  // part 1
  def count1(in: String) = {
    val m = parseMatrix(in)
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
