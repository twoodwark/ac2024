import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
import scala.collection.immutable
/*

 */
@main def Day18(size: Int) =

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
    while toVisit.nonEmpty && toVisit.head != end
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
  val maxX, maxY = size
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

  def fall(xys: Seq[XY], on: Map[XY, Char]) =
    on ++ xys.map(_ -> BLOCK)

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

  val startXY = (0, 0)
  val endXY = (maxX, maxY)
  val bytes = parse
  def part1 =
    val empty = for
      x <- startXY._1 to endXY._1
      y <- startXY._2 to endXY._2
    yield (x, y) -> FREE

    val grid = fall(bytes.take(1024), empty.toMap.withDefaultValue(OUT))
    given Map[XY, Char] = grid
    printGrid

    val cost = aStar(
      GridN(startXY),
      GridN(endXY),
      heuristicCost = (n1, n2) => n1.xy.manhattan(n2.xy)
    )
    cost.get.toLong

  part1.pipe(println)
