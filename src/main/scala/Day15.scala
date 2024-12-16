import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
/*
 */
object Day15 extends App {
  final val WALL = '#'
  final val BOT = '@'
  final val BOX = 'O'
  final val NOTHING = '.'
  val (state, instructions) = parse

  part1.pipe(println)

  type XY = (Int, Int)
  extension [T](c: XY)(using what: Map[XY, T])
    def moveBy(d: XY): XY = (c(0) + d(0), c(1) + d(1))
    def value: T = what(c)

  case class State(
      grid: Map[XY, Char],
      botLoc: XY
  ) {
    def print_() =
      given Map[XY, Char] = grid
      val (maxX, maxY) =
        (grid.keysIterator.map(_._1).max, grid.keysIterator.map(_._2).max)
      for
        y <- 0 to maxX
        x <- 0 to maxY
      do
        if x == 0 then println()
        val value = (x, y).value
        print(value)
      println()

    def swap(from: XY, to: XY) =
      println(f"swqp $from $to")
      val (oldF, oldT) = (grid(from), grid(to))
      val g = grid.updated(from, oldT).updated(to, oldF)
      State(g, botLoc)

    def applyInstruction(instruction: XY): State =
      given Map[XY, Char] = grid.withDefaultValue(NOTHING)
      val contiguousNextLocs = Iterator
        .iterate(botLoc)(_.moveBy(instruction))
        .takeWhile(_.value != NOTHING)
        .toList
      val canMoveInDir = !contiguousNextLocs.tail.map(_.value).contains(WALL)
      if !canMoveInDir then return this
      val newLoc = botLoc.moveBy(instruction)
      // swap nothing with box, then repeat, etc. until nothing is at current botLoc
      contiguousNextLocs.foldRight(State(grid, newLoc)) { (cc, s) =>
        s.swap(cc, cc.moveBy(instruction))
      }

    def sumGPScoordinates =
      grid.filter(_._2 == BOX).keys.map { (x, y) => 100 * y + x }.sum
  }

  def part1 =
    state.print_()
    val finalState = instructions.foldLeft(state) { (s, c) =>
      s.applyInstruction(c)
    }
    finalState.print_()
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
