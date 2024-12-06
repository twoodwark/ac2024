import scala.io.Source
import scala.collection.mutable
import scala.util.chaining._

/*
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...

 */
object Day6 {
  type XY = (Int, Int)
  val dirs = IndexedSeq((0, -1), (1, 0), (0, 1), (-1, 0))
  def rotRight(d: Int) = (d + 1) % dirs.length

  extension (c: XY) def moveBy(d: XY): XY = (c(0) + d(0), c(1) + d(1))
  val OUT = '_'
  def main(args: Array[String]): Unit = {
    val mat = parseMatrix
    println(count1(mat))
  }

  def parseMatrix: Map[XY, Char] = {
    Source.stdin.mkString.linesIterator.zipWithIndex
      .flatMap { (row, y) =>
        row.zipWithIndex.map { (c, x) => ((x, y), c) }
      }
      .pipe(Map.from)
      .withDefaultValue(OUT)
  }
//   def parseMatrix: ArraySeq[ArraySeq[Char]] = {
//     // TODO why use array not map?
//     Source.stdin.mkString.linesIterator
//       .map((l) => ArraySeq.from(l.toCharArray))
//       .pipe(ArraySeq.from)
//   }

  // part 1
  def count1(mat: Map[XY, Char]) = {
    val start = mat.collectFirst { case (coord, '^') => coord }.head
    def next(currentDir: Int, pos: XY, visited: Set[XY]): Set[XY] = {
      val at = mat(pos)
      if (at == OUT) visited // end
      else {
        val nextPos = pos.moveBy(dirs(currentDir))
        if (mat(nextPos) == '#') next(rotRight(currentDir), pos, visited + pos)
        else next(currentDir, nextPos, visited + pos)
      }
    }
    next(0, start, Set()).size
  }
}
