import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
import scala.annotation.tailrec
/*
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
 */

type XY = (Int, Int)
extension (c: XY)(using what: Map[XY, Int])
  def moveBy(d: XY): XY = (c(0) + d(0), c(1) + d(1))
  def value = what(c)
  def neighbours: Set[XY] =
    val h = value
    Set(moveBy(1, 0), moveBy(0, -1), moveBy(-1, 0), moveBy(0, 1))
      .filter { o => h + 1 == o.value }

object Day10 extends App {
  given Map[XY, Int] = parseMatrix
  val HEAD = 0
  val SUMMIT = 9
  part1.pipe(println)
  part2.pipe(println)

  def part1(using mat: Map[XY, Int]) =
    val heads = mat.keySet.filter(_.value == HEAD).toList
    // recursive BFS
    @tailrec def count(
        current: Set[XY],
        visited: Set[XY] = Set(),
        total: Int = 0
    ): Int =
      if current.isEmpty then return total
      val found = current.filter { _.value == SUMMIT }
      var check = current.flatMap(neighbours) -- visited
      count(
        check,
        check ++ visited,
        total + found.size
      )
    heads.map(Set(_)).map(count(_)).sum

  def part2(using mat: Map[XY, Int]) =
    val heads = mat.keySet.filter(_.value == HEAD).toList
    // err dfs
    var cache = mutable.Map[XY, Long]()
    def countDistinct(from: XY): Long =
      cache.getOrElseUpdate(
        from, {
          val (found, check) = from.neighbours.partition { _.value == SUMMIT }
          // current node has found.size distinct paths
          // plus any paths from check
          found.size + check.toList.map(countDistinct).sum
        }
      )
    heads.map(countDistinct).sum

  def parseMatrix =
    Source.stdin.mkString.linesIterator.zipWithIndex
      .flatMap { (row, y) =>
        row.zipWithIndex.map { (c, x) => ((x, y), c.toString().toInt) }
      }
      .pipe(Map.from)
      .withDefaultValue(-1)

}
