import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
import scala.collection.SortedSet
import scala.collection.immutable
/*
 */
object Day15 extends App {
  val WALL = '#'
  val BOT = '@'
  val BOX = 'O'
  val BOX_L = '['
  val BOX_R = ']'
  val NOTHING = '.'
  val (state, instructions) = parse

  part1.pipe(println)
  part2.pipe(println)

  type XY = (Int, Int)
  extension [T](c: XY)(using what: Map[XY, T])
    def moveBy(d: XY): XY = (c(0) + d(0), c(1) + d(1))
    def value: T = what(c)

  case class State(
      grid: Map[XY, Char],
      botLoc: XY
  ) {
    assert(grid(botLoc) == BOT)
    def print_() =
      given Map[XY, Char] = grid
      val (maxX, maxY) =
        (grid.keysIterator.map(_._1).max, grid.keysIterator.map(_._2).max)
      for
        y <- 0 to maxY
        x <- 0 to maxX
      do
        if x == 0 then println()
        print((x, y).value)
      println()

    def doubleWide =
      State(
        grid.flatMap { (xy, c) =>
          val (l, r) = c match {
            case BOT => (BOT, NOTHING)
            case BOX => (BOX_L, BOX_R)
            case _   => (c, c)
          }
          Seq(
            (xy._1 * 2, xy._2) -> l,
            (xy._1 * 2 + 1, xy._2) -> r
          )
        },
        (botLoc._1 * 2, botLoc._2)
      )

    def applyInstruction(instruction: XY): State =
      given Map[XY, Char] = grid.withDefaultValue(NOTHING)
      def tileSwaps(xy: XY): Seq[(XY, XY)] =
        // for horizontal moves we always have 1 neighbour
        // for vertical moves we may have 1 or 2
        val moveTile = xy -> xy.moveBy(instruction)
        (xy.value, instruction) match {
          case (BOX_L, (0, _)) =>
            Seq(
              moveTile,
              xy.moveBy(1, 0) -> xy.moveBy(1, 0).moveBy(instruction)
            )
          case (BOX_R, (0, _)) =>
            Seq(
              moveTile,
              xy.moveBy(-1, 0) -> xy.moveBy(-1, 0).moveBy(instruction)
            )
          case _ => Seq(moveTile)
        }
      // recursively get things that need to move
      def nextTileSwaps(from: XY): Seq[(XY, XY)] =
        val willMove = tileSwaps(from)
        // terminate on walls, return them so we can find them
        if willMove.exists(_._2.value == WALL) then willMove
        else
          willMove ++ willMove.flatMap {
            case (_, r) if r.value != NOTHING => nextTileSwaps(r)
            case _                            => Seq()
          }

      // Reverse order then dedup!
      val swaps = immutable.ListSet.from(nextTileSwaps(botLoc).reverse)
      if swaps.exists(_._2.value == WALL) then return this
      assert(swaps.head._2.value == NOTHING, f"swaps ${swaps.mkString}")
      def swap(g: Map[XY, Char], from: XY, to: XY) =
        val (oldF, oldT) = (g(from), g(to))
        g ++ Seq(from -> oldT, to -> oldF)

      val newGrid = swaps.foldLeft(grid) { (g, sw) => swap(g, sw._1, sw._2) }
      State(newGrid, botLoc.moveBy(instruction))

    def sumGPScoordinates =
      grid.collect { case ((x, y), BOX | BOX_L) => 100 * y + x }.sum
  }

  def part1 =
    val finalState = instructions.foldLeft(state) { (s, c) =>
      s.applyInstruction(c)
    }
    finalState.sumGPScoordinates

  def part2 =
    val wideState = state.doubleWide
    val finalState = instructions.foldLeft(wideState) { (s, c) =>
      s.applyInstruction(c)
    }
    finalState.sumGPScoordinates

  def parse =
    val Array(mapStr, inStr) = Source.stdin.mkString.split("\n\n")
    val grid = mapStr.linesIterator.zipWithIndex.flatMap { (l, y) =>
      l.zipWithIndex.map { (value, x) => ((x, y), value) }
    }.toMap
    val botLoc = grid.keys.find(xy => grid(xy) == BOT).head
    val instructions: Seq[XY] =
      inStr.replaceAll("\n", "").toIndexedSeq.collect {
        case '>' => (1, 0)
        case '<' => (-1, 0)
        case 'v' => (0, 1)
        case '^' => (0, -1)
      }

    (State(grid, botLoc), instructions)
}
