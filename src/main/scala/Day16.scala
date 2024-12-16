import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
import scala.collection.immutable
/*
 */
object Day16 extends App {
  val WALL = '#'
  val START = 'S'
  val END = 'E'
  val DIRS = Seq(
    (1, 0), // East
    (0, -1),
    (-1, 0),
    (0, 1)
  )
  val (startNode, endNodes) = parse

  part1.pipe(println)

  trait Node[Self <: Node[?]] {
    def nextNodesWithCost: Map[Self, Double]
  }

  def dijkstra[N <: Node[N]](start: N, end: Set[N]): (Seq[N], Double) =
    val dists =
      mutable.Map[N, Double]().withDefaultValue(Double.PositiveInfinity)
    dists += start -> 0d
    // Not to be processed
    val visited = mutable.Set[N]()
    // Nodes mapped to the lowest-dist neighbour that leads to it
    val pathFrom = mutable.Map[N, N]()
    // min-heap
    case class QueuedNode(n: N, dist: Double) extends Ordered[QueuedNode]:
      def compare(that: QueuedNode) = -this.dist.compare(that.dist)

    val toVisit = mutable.PriorityQueue[QueuedNode](QueuedNode(start, 0d))
    def lowest = end.minBy(dists(_))
    while {
      toVisit.nonEmpty && toVisit.head.dist < dists(lowest)
    }
    do {
      val QueuedNode(node, dist) = toVisit.dequeue()
      visited += node
      for (child, d) <- node.nextNodesWithCost
      do
        val totalDist = d + dist
        if totalDist < dists(child) then
          dists += (child -> totalDist)
          pathFrom += (child -> node)
        if !visited.contains(child) then toVisit += QueuedNode(child, totalDist)
    }
    val endAt = lowest
    val path = start ::
      Iterator
        .iterate(endAt)(pathFrom.apply)
        .takeWhile(_ != start)
        .toList
        .reverse
    (path, dists(endAt))

  type XY = (Int, Int)
  extension [T](c: XY)(using what: Map[XY, T])
    def moveBy(d: XY): XY = (c(0) + d(0), c(1) + d(1))
    def value: T = what(c)

  case class GridN(
      dir: Int, // 0=E
      xy: (Int, Int)
  )(using map: Map[(Int, Int), Char])
      extends Node[GridN]:
    type Self = GridN
    def nextNodesWithCost: Map[GridN, Double] =
      val rotated = Map(
        copy(dir = (dir + 1 + DIRS.size) % DIRS.size) -> 1000d,
        copy(dir = (dir - 1 + DIRS.size) % DIRS.size) -> 1000d
      )
      val next = xy.moveBy(DIRS(dir))
      if next.value == WALL then rotated
      else rotated + (copy(xy = next) -> 1d)

  def part1 =
    val x = startNode.nextNodesWithCost
    val (path, cost) = dijkstra(startNode, endNodes)
    cost.toLong

  def parse =
    val grid = Source.stdin.mkString.linesIterator.zipWithIndex.flatMap {
      (l, y) =>
        l.zipWithIndex.map { (value, x) => ((x, y), value) }
    }.toMap
    given Map[(Int, Int), Char] = grid
    val startLoc = grid.keys.find(xy => grid(xy) == START).head
    val endLoc = grid.keys.find(xy => grid(xy) == END).head
    val startNode = GridN(DIRS.indexOf((1, 0)), startLoc)
    val endNodes =
      for d <- DIRS.indices
      yield GridN(d, endLoc)
    (startNode, endNodes.toSet)
}
