import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
/*
 */
object Day13 extends App {
  val machines = parse
  val A_COST = 3
  val B_COST = 1
  val MAX_TIMES_EACH_BUTTON = 100
  part1.pipe(println)

  // Feels like we should: simplex algorithm, dijkstra??!
  case class Machine(
      ax: Long,
      ay: Long,
      bx: Long,
      by: Long,
      px: Long,
      py: Long
  ) {
    // 0 means not solved
    def minCost: Long =
      /*
      // simultaneous equation
        total_cost = 3 * a + 1 * b
        px = a * ax + b * bx
        py = a * ay + b * by
        (py - b * by)/ay = a
        total_cost = 3 * (py - b * by) / ay + 1 * b
       */

      // computes exact cost of solution returns a, cost
      def total_cost(b: Int): Option[(Int, Long)] =
        // a has to be an integer
        val a_times_ay = py - b * by
        if a_times_ay % ay != 0 then return None
        val a = (a_times_ay / ay).toInt
        if a <= MAX_TIMES_EACH_BUTTON && px == a * ax + b * bx then
          Some((a, A_COST * a + B_COST * b))
        else None

      val first = (0 to MAX_TIMES_EACH_BUTTON)
        .map { b => (b, total_cost(b)) }
        .collect { case (b, Some((a, c))) => (a, b, c).tap(println) }
        .sortBy(_._3)
        .headOption

      first.map(_._3).getOrElse(0)
  }

  def part1 =
    machines.map(_.minCost.tap(println)).sum

  def parse =
    Source.stdin.mkString
      .split("\n\n")
      .collect { (s) =>
        s.strip() match {
          case (s"Button A: X+${ax}, Y+${ay}\nButton B: X+${bx}, Y+${by}\nPrize: X=${px}, Y=${py}") =>
            Machine(
              ax.toLong,
              ay.toLong,
              bx.toLong,
              by.toLong,
              px.toLong,
              py.toLong
            )
        }
      }
      .toSeq

}
