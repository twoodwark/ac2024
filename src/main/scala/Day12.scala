import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.View.Iterate
/*
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
 */
type XY = (Int, Int)
extension [T](c: XY)(using what: Map[XY, T])
  def moveBy(d: XY): XY = (c(0) + d(0), c(1) + d(1))
  def value: T = what(c)
  def neighbours: Set[XY] =
    Set(moveBy(1, 0), moveBy(0, -1), moveBy(-1, 0), moveBy(0, 1))

case class Region(
    char: Char,
    val cells: mutable.Set[XY] = mutable.Set(),
    var perimeter: Int = 0
) {
  def cost = perimeter * area
  def area = cells.size
  def mergeAndReturnIncludable(add: Set[XY])(using Map[XY, Char]): Set[XY] =
    add.flatMap { cell =>
      assert(cell.value == char)
      val (nextInclude, cellPerimeter) =
        (cell.neighbours -- cells).partition(_.value == char)
      perimeter += cellPerimeter.size
      cells += cell
      nextInclude
    }
}

object Day12 extends App {
  given Map[XY, Char] = parseMap
  part1.pipe(println)

  def part1(using map: Map[XY, Char]) =
    // flood fill??
    def createRegionFrom(cell: XY) =
      val r = Region(cell.value)
      Iterator.iterate(Set(cell))(r.mergeAndReturnIncludable).find(_.isEmpty)
      r

    var queue = mutable.Set.from(map.keySet)
    Iterator
      .continually(queue.headOption)
      .takeWhile(_.isDefined)
      .collect { case Some(c) =>
        createRegionFrom(c).tap { queue --= _.cells }
      }
      .map(_.cost)
      .sum

  def parseMap =
    Source.stdin.mkString.linesIterator.zipWithIndex
      .flatMap { (row, y) =>
        row.zipWithIndex.map { (c, x) => ((x, y), c) }
      }
      .pipe(Map.from)
      .withDefaultValue('?')

}
