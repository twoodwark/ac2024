import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
/*
 */
object Day14 extends App {
  val bots = parse
  val WIDTH = 101
  val HEIGHT = 103
  val SEC = 100

  part1.pipe(println)
  case class Bot(
      p: (Int, Int),
      v: (Int, Int)
  ) {
    def step(times: Int) =
      val dx = (WIDTH + v._1) % WIDTH
      val dy = (HEIGHT + v._2) % HEIGHT
      Bot(
        (
          (p._1 + times * dx) % WIDTH,
          (p._2 + times * dy) % HEIGHT
        ),
        v
      )
      // 01
      // 23
    def quadrant =
      if p._1 < WIDTH / 2 && p._2 < HEIGHT / 2 then Some(0)
      else if p._1 > WIDTH / 2 && p._2 < HEIGHT / 2 then Some(1)
      else if p._1 < WIDTH / 2 && p._2 > HEIGHT / 2 then Some(2)
      else if p._1 > WIDTH / 2 && p._2 > HEIGHT / 2 then Some(3)
      else None
  }

  def part1 =
    def print_(bts: Seq[Bot]) = {
      for
        y <- 0 until HEIGHT
        x <- 0 until WIDTH
      do
        if x == 0 then println()
        val count = bts.count(_.p == (x, y))
        print(if count > 0 then count else '.')
      println()
    }

    val finalBots = bots.tap(print_).map(_.step(SEC)).tap(print_)
    finalBots
      .groupBy(_.quadrant)
      .removed(None)
      .view
      .mapValues(_.size)
      .toMap
      .tap(println)
      .values
      .product

  def parse =
    Source.stdin.mkString.linesIterator.collect { (s) =>
      s.strip() match {
        case (s"p=${px},${py} v=${vx},${vy}") =>
          Bot((px.toInt, py.toInt), (vx.toInt, vy.toInt))
      }
    }.toSeq

}