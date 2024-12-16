import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
/*
# ans: 15,150000000015
Button A: X+200, Y+200
Button B: X+10, Y+10
Prize: X=1000, Y=1000

# ans 0,0
Button A: X+30, Y+20
Button B: X+15, Y+10
Prize: X=1000, Y=1000
 */
object Day13 extends App {
  val machines = parse
  val A_COST = 3
  val B_COST = 1
  val MAX_TIMES_EACH_BUTTON = 100L
  def cost(a: Long, b: Long) =
    A_COST * a + B_COST * b

  part1.pipe(println)
  part2.pipe(println)

  case class Machine(
      ax: Long,
      ay: Long,
      bx: Long,
      by: Long,
      px: Long,
      py: Long
  ) {
    def initial =
      /*
      px = a * ax + b * bx
      py = a * ay + b * by
      px = ax*(py - b * by)/ay + b * bx

      exact solution
      px/bx - a * ax/bx = py/by - a * ay/by
      px * by - a * ax * by = py * bx - a * ay * bx
      px * by - py * bx = a * (ax * by - ay * bx)
       */
      // By construction py == a * ay + b * by using this exact value of a
      // where A and B are colinear, we get divide by zero, for Infinity.
      // if P is colinear with A we get NaN -> zero here.
      val exactA =
        (px * by - py * bx).toDouble / (ax * by - ay * bx)
      val exactB =
        (px * ay - py * ax).toDouble / (bx * ay - by * ax)
      Seq(
        (exactA.floor.toLong * ay, py - exactA.floor.toLong * ay),
        (py - exactB.floor.toLong * by, exactB.floor.toLong * by)
      ).flatMap { (abyAy, bbyBy) =>
        val a = abyAy / ay
        val b = bbyBy / by
        if a >= 0 && b >= 0 && bbyBy % by == 0 && abyAy % ay == 0 && px == a * ax + b * bx
        then
          assert(
            py == a * ay + b * by,
            s"$py = $a * $ay + $b * $by for ${this}"
          )
          val c = cost(a, b)
          Some((a, b, c))
        else None
      }

    def minCost =
      initial
        .minByOption(_._3) // in case of colinear
        .getOrElse((0L, 0L, 0L))
  }

  def part1 =
    machines
      .map(_.minCost)
      .map { (a, b, c) =>
        if (a <= MAX_TIMES_EACH_BUTTON && b <= MAX_TIMES_EACH_BUTTON) then c
        else 0
      }
      .sum
  def part2 =
    machines
      .map {
        case Machine(
              ax,
              ay,
              bx,
              by,
              px,
              py
            ) =>
          Machine(
            ax,
            ay,
            bx,
            by,
            px + 10000000000000L,
            py + 10000000000000L
          )
      }
      .map(_.minCost._3)
      .sum

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
