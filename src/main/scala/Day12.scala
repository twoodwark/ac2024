import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.View.Iterate
/*
 */
type XY = (Int, Int)
extension [T](c: XY)(using what: Map[XY, T])
  def moveBy(d: XY): XY = (c(0) + d(0), c(1) + d(1))
  def value: T = what(c)
  def neighbours: Seq[XY] =
    Seq(moveBy(1, 0), moveBy(0, -1), moveBy(-1, 0), moveBy(0, 1))
  // left, centre, right of the vertex in that direction (0-3)
  def vertexNeighbours(dir: Int): Seq[XY] =
    val l = neighbours(dir) // up
    val c = l.neighbours((dir + 3) % 4) // right
    val r = c.neighbours((dir + 2) % 4) // down
    Seq(l, c, r)

case class Region(
    char: Char,
    val cells: mutable.Set[XY] = mutable.Set(),
    var perimeter: Int = 0
)(using Map[XY, Char]) {
  def cost = perimeter * area
  def area = cells.size
  def mergeAndReturnIncludable(add: Set[XY]): Set[XY] =
    add.flatMap { cell =>
      val (nextInclude, cellPerimeter) =
        (cell.neighbours.toSet -- cells).partition(_.value == char)
      perimeter += cellPerimeter.size
      cells += cell
      nextInclude
    }
  def costDiscounted = numSides * area
  def numSides =
    // a vertex of a cell x adds a side if it has
    // X?   or  ??   or  ?X
    // xX       x?       x?

    // each cell can have up to four such vertices
    (for
      cell <- cells.toList
      dir <- 0 to 3
    yield {
      val sameAtVertex =
        cell.vertexNeighbours(dir).map(_.value == cell.value)
      sameAtVertex match
        case Seq(true, false, true) => 1
        case Seq(false, _, false)   => 1
        case _                      => 0
    }).sum

}

object Day12 extends App {
  given Map[XY, Char] = parseMap
  part1.pipe(println)
  part2.pipe(println)

  def part1(using map: Map[XY, Char]) =
    regions.map(_.cost).sum
  def part2(using map: Map[XY, Char]) =
    regions.map(_.costDiscounted).sum

  def regions(using map: Map[XY, Char]) =
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

  def parseMap =
    Source.stdin.mkString.linesIterator.zipWithIndex
      .flatMap { (row, y) =>
        row.zipWithIndex.map { (c, x) => ((x, y), c) }
      }
      .pipe(Map.from)
      .withDefaultValue('?')

}
