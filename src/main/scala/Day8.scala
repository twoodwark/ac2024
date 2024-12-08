import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
/*
 */
object Day8 {
  type XY = (Int, Int)
  def main(args: Array[String]): Unit =
    val (locs, maxXY) = parseAnttennaLocationsAndMapDimensions
    part1(locs, maxXY).pipe(println)
    part2(locs, maxXY).pipe(println)

  def parseAnttennaLocationsAndMapDimensions: (Map[Char, List[XY]], XY) = {
    val in = Source.stdin.getLines.toArray
    val maxXY = (in(0).length - 1, in.length - 1)
    val locs = mutable.Map[Char, List[XY]]().withDefaultValue(List())
    for
      (row, y) <- in.zipWithIndex
      (c, x) <- row.zipWithIndex
      if c != '.'
    do
      val xy = (x, y)
      locs += ((c, xy :: locs(c)))
    (locs.toMap, maxXY)
  }

  def add(c1: XY, c2: XY) =
    (c1._1 + c2._1, c1._2 + c2._2)

  def gradient(c1: XY, c2: XY) =
    add((-c1._1, -c1._2), c2)

  def antinodes1(c1: XY, c2: XY) =
    if c1 == c2 then throw Exception()
    val (dx, dy) = gradient(c1, c2)
    Set(add((dx, dy), c2), add((-dx, -dy), c1))

    // oops! just use List.combinations
  def allPairs[T](l: List[T]): Iterable[(T, T)] =
    var rest = l
    val r = mutable.ListBuffer[(T, T)]()
    while rest.nonEmpty
    do
      for (n <- rest.tail)
      do r.addOne((rest.head, n))
      rest = rest.tail
    r.view

  def part1(locs: Map[Char, List[XY]], maxXY: XY): Int = {
    val foundNodes = for
      coords <- locs.values
      (c1, c2) <- allPairs(coords)
      (x, y) <- antinodes1(c1, c2)
      if x >= 0 && y >= 0 && x <= maxXY._1 && y <= maxXY._2
    yield (x, y)

    foundNodes.toSet.size
  }

  def part2(locs: Map[Char, List[XY]], maxXY: XY): Int = {
    val (maxX, maxY) = maxXY
    def okNode(x: Int, y: Int) =
      x >= 0 && y >= 0 && x <= maxX && y <= maxY
    def antinodes2(c1: XY, c2: XY) =
      if c1 == c2 then throw Exception()
      val (dx, dy) = gradient(c1, c2)
      val c1nodes = Iterator
        .from(0)
        .map { i => add((-i * dx, -i * dy), c1) }
        .takeWhile(okNode)
      val c2nodes = Iterator
        .from(0)
        .map { i => add((i * dx, i * dy), c2) }
        .takeWhile(okNode)
      c1nodes.concat(c2nodes)

    val foundNodes = for
      coords <- locs.values
      (c1, c2) <- allPairs(coords)
      (x, y) <- antinodes2(c1, c2)
    yield (x, y)

    foundNodes.toSet.size
  }
}
