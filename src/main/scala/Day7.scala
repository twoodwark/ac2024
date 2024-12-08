import scala.io.Source
import scala.util.chaining._

/*
 */
object Day7 {
  type Problem = (Long, List[Long])
  def main(args: Array[String]): Unit = {
    val problems = parseProblems
    // part 1
    count(problems, Array(_ + _, _ * _)).pipe(println)
    // part 2
    count(
      problems,
      Array(_ + _, _ * _, (a, b) => (a.toString + b.toString).toLong)
    ).pipe(println)
  }

  def parseProblems =
    Source.stdin
      .getLines()
      .map { (l) =>
        val test :: rest =
          raw"\d+".r.findAllIn(l).map(_.toLong).toList: @unchecked
        (test, rest)
      }
      .toList

  def count(problems: List[Problem], ops: Array[(Long, Long) => Long]) = {
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
