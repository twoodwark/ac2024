import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
import scala.collection.immutable
/*
 */
@main def Day18 =

  trait Node[Self <: Node[?]]:
    def nextNodesWithCost: Map[Self, Double]

  def aStar[N <: Node[N]](
      start: N,
      end: N,
      heuristicCost: (N, N) => Double = (_: N, _: N) =>
        0d // default  = dijkstra?
  ): Option[Double] =
    assert(end != start)
    assert(heuristicCost(end, end) == 0)
    assert(heuristicCost(end, start) > 0)
    // Total edge cost to get to the given node
    val totalCost =
      mutable.Map[N, Double]().withDefaultValue(Double.PositiveInfinity)
    totalCost += start -> 0d
    // Not to be processed
    val visited = mutable.Set[N]()
    // Nodes mapped to every lowest-total neighbour that leads to it
    val pathTo =
      mutable.Map[N, mutable.Set[N]]().withDefault(_ => mutable.Set())
    // min-heap
    case class QueuedNode(n: N, total: Double) extends Ordered[QueuedNode]:
      def compare(that: QueuedNode) = -this.total.compare(that.total)
    val toVisit = mutable.PriorityQueue[QueuedNode](QueuedNode(start, 0d))
    while toVisit.nonEmpty && toVisit.head.n != end
    do
      val QueuedNode(node, _) = toVisit.dequeue()
      if !visited.contains(node) then
        visited += node
        for (child, edgeCost) <- node.nextNodesWithCost
        do
          val childTotalCost = edgeCost + totalCost(node)
          if childTotalCost < totalCost(child) then // <= for multi path
            totalCost(child) = childTotalCost
            pathTo(child) =
              // parents would have had lower or equal total than current
              // node when previously added to pathTo(child); but their edgeCost
              // could have been higher.
              pathTo(child).addOne(node)
          if !visited.contains(child) then
            val hCost = heuristicCost(child, end)
            toVisit += QueuedNode(child, childTotalCost + hCost)
    if totalCost(end) == Double.PositiveInfinity then None
    else Some(totalCost(end))

  type XY = (Int, Int)
  extension [T](c: XY)(using what: Map[XY, T])
    def moveBy(d: XY): XY = (c(0) + d(0), c(1) + d(1))
    def value: T = what(c)
    def manhattan(d: XY): Int = (d._1 - c._1).abs + (d._2 - c._2).abs
  val DIRS = Seq(
    (1, 0),
    (0, -1),
    (-1, 0),
    (0, 1)
  )
  val OUT = '?'
  val BLOCK = '#'
  val FREE = '.'
  case class GridN(xy: XY)(using Map[XY, Char]) extends Node[GridN]:
    def nextNodesWithCost =
      DIRS
        .map(xy.moveBy)
        .filter(_.value == FREE)
        .map: n =>
          copy(xy = n) -> 1d
        .toMap

  def parse =
    Source.stdin
      .getLines()
      .map: l =>
        val Array(x, y) = l.split(",").map(_.toInt)
        (x, y)
      .toSeq

  def printGrid(using what: Map[XY, Char]) =
    val maxX = what.keys.map(_._1).max
    val maxY = what.keys.map(_._2).max
    for
      y <- (0 to maxY)
      x <- (0 to maxX)
    do
      if x == 0 then println()
      print((x, y).value)
    println()

  val bytes = parse
  val maxX, maxY, size = bytes.map(_.max(_)).max
  val startXY = (0, 0)
  val endXY = (maxX, maxY)
  val empty = (for
    x <- startXY._1 to maxX
    y <- startXY._2 to maxY
  yield (x, y) -> FREE).toMap.withDefaultValue(OUT)

  def getGrid(n: Int) =
    empty ++ bytes.take(n).map(_ -> BLOCK)

  def part1 =
    val take = if size == 6 then 12 else 1024
    val grid = getGrid(take)
    given Map[XY, Char] = grid
    printGrid

    val cost = aStar(
      GridN(startXY),
      GridN(endXY),
      heuristicCost = (n1, n2) => n1.xy.manhattan(n2.xy)
    )
    cost.get.toLong

  def part2 =
    def solve(grid: Map[XY, Char]) =
      given Map[XY, Char] = grid
      aStar(
        GridN(startXY),
        GridN(endXY),
        heuristicCost = (n1, n2) => n1.xy.manhattan(n2.xy)
      ).isDefined
    var range = (0, bytes.length - 1) // (solved, unsolved)
    def mid = range._1 + (range._2 - range._1) / 2
    while mid > range._1
    do
      val isSolved = solve(getGrid(mid))
      if isSolved then range = (mid, range._2)
      else range = (range._1, mid)
    printGrid(using getGrid(range._1))
    bytes(range._1)

  part1.pipe(println)
  part2.pipe(println)
