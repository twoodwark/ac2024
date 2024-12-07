import scala.io.Source
import scala.util.chaining._

/*
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20

 */
object Day7 {
  type Problem = (Long, List[Long])
  val ops = Array((a: Long, b: Long) => a + b, (a: Long, b: Long) => a * b)
  def main(args: Array[String]): Unit = {
    val problems = parseProblems
    println(part1(problems))
  }

  def parseProblems = {
    Source.stdin
      .getLines()
      .map { (l) =>
        val test :: rest =
          raw"\d+".r.findAllIn(l).map(_.toLong).toList: @unchecked
        (test, rest)
      }
      .toList
  }

  def part1(problems: List[Problem]) = {
    // recursive DFS
    def solve(test: Long, acc: Long, values: List[Long]): Boolean = {
      if (values.isEmpty) return test == acc
      ops.exists { (o) => solve(test, o(acc, values.head), values.tail) }
    }
    problems
      .filter { (test, values) => solve(test, values.head, values.tail) }
      .map(_._1)
      .sum
  }
}
