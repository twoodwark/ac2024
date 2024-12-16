import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
/*
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176
 */
object Day13 extends App {
  val machines = parse
  val A_COST = 3
  val B_COST = 1
  val MAX_TIMES_EACH_BUTTON = 100
  def cost(a: Long, b: Long) =
    A_COST * a + B_COST * b

  part1.pipe(println)
  // 25679666644134: too low - must be failing to solve some!
  // 25286589183420: too low
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
      val exactA =
        (px * by - py * bx).toDouble / (ax * by - ay * bx)
      val exactB =
        (px * ay - py * ax).toDouble / (bx * ay - by * ax)
      Seq(
        (exactA.ceil.toLong * ay, py - exactA.ceil.toLong * ay),
        (exactA.floor.toLong * ay, py - exactA.floor.toLong * ay),
        (py - exactB.ceil.toLong * by, exactB.ceil.toLong * by),
        (py - exactB.floor.toLong * by, exactB.floor.toLong * by)
      ).flatMap { (abyAy, bbyBy) =>
        val a = abyAy / ay
        val b = bbyBy / by
        if abyAy >= 0 && bbyBy >= 0 && bbyBy % by == 0 && abyAy % ay == 0 && px == a * ax + b * bx
        then
          assert(
            px == a * ax + b * bx,
            s"$px = $a * $ax + $b * $bx for ${this}"
          )
          assert(
            py == a * ay + b * by,
            s"$py = $a * $ay + $b * $by for ${this}"
          )
          // println(s"$px = $a * $ax + $b * $bx")
          // println(s"$py = $a * $ay + $b * $by")
          val c = cost(a, b)
          Some((a, b, c))
        else
          // println("no")
          None
      }.headOption

    def minCost =
      // val max_a = (py / ay).min(px / ax)
      // val max_b = (py / by).min(px / bx)
      // /*
      // // simultaneous equation
      //   total_cost = 3 * a + 1 * b
      //   px = a * ax + b * bx
      //   py = a * ay + b * by
      //   (py - b * by)/ay = a
      //   total_cost = 3 * (py - b * by) / ay + 1 * b
      //  */

      // // computes exact cost of solution returns a, cost
      // def total_cost(b: Int): Option[(Int, Long)] =
      //   // a has to be an integer
      //   val a_times_ay = py - b * by
      //   if a_times_ay % ay != 0 then return None
      //   val a = (a_times_ay / ay).toInt
      //   if a <= MAX_TIMES_EACH_BUTTON && px == a * ax + b * bx then
      //     Some((a, A_COST * a + B_COST * b))
      //   else None

      // val first = (0 to MAX_TIMES_EACH_BUTTON)
      //   .map { b => (b, total_cost(b)) }
      //   .collect { case (b, Some((a, c))) => (a, b, c).tap(println) }
      //   .sortBy(_._3)
      //   .headOption

      // first.map(_._3).getOrElse(0)
      initial.getOrElse((0L, 0L, 0L))
  }

  def part1 =
    machines
      .map(_.minCost)
      .map { (a, b, c) =>
        if (
          a <= MAX_TIMES_EACH_BUTTON.toLong && b <= MAX_TIMES_EACH_BUTTON.toLong
        ) then c
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
      .map(_.minCost._2)
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
