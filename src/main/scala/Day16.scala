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
  part2.pipe(println)

  def part1 =
    val (_, cost) = dijkstra(startNode, endNodes)
    cost.toLong
  def part2 =
    val (bestPaths, _) = dijkstra(startNode, endNodes)
    println(
      s"${bestPaths.size} best paths from ${startNode} to ${endNodes}"
    )
    println(bestPaths(0))

    bestPaths.flatten.map(_.xy).toSet.size

  trait Node[Self <: Node[?]] {
    def nextNodesWithCost: Map[Self, Double]
  }

  def dijkstra[N <: Node[N]](start: N, end: Set[N]): (Seq[Seq[N]], Double) =
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
    def leastTotalCost = end.map(totalCost).min
    while {
      toVisit.nonEmpty && toVisit.head.total < leastTotalCost
    }
    do {
      val QueuedNode(node, total) = toVisit.dequeue()
      visited += node
      for (child, edgeCost) <- node.nextNodesWithCost
      do
        val childTotalCost = edgeCost + total
        if childTotalCost <= totalCost(child) then
          totalCost(child) = childTotalCost
          pathTo(child) =
            // parents would have had lower or equal total than current
            // node when previously added to pathTo(child); but their edgeCost
            // could have been higher.
            pathTo(child).addOne(node)
        if !visited.contains(child) then
          toVisit += QueuedNode(child, childTotalCost)
    }
    def allPaths(to: N): Seq[Seq[N]] =
      val pathThis = Seq(to)
      pathTo.get(to) match
        case None          => Seq(pathThis) // a single path
        case Some(parents) =>
          // append this node to every path to a parent node
          parents.toSeq.flatMap(allPaths).map(_ ++ pathThis)
    val bestPaths =
      end.toSeq.filter(totalCost(_) == leastTotalCost).flatMap(allPaths)
    (bestPaths, leastTotalCost)

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

  def parse =
    val grid = Source.stdin.mkString.linesIterator.zipWithIndex.flatMap {
      (l, y) =>
        l.zipWithIndex.map { (value, x) => ((x, y), value) }
    }.toMap
    given Map[(Int, Int), Char] = grid
    val startLoc = grid.keys.find(_.value == START).head
    val endLoc = grid.keys.find(_.value == END).head
    val startNode = GridN(DIRS.indexOf((1, 0)), startLoc)
    val endNodes =
      for d <- DIRS.indices
      yield GridN(d, endLoc)
    (startNode, endNodes.toSet)
}
