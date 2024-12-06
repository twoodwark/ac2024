import scala.io.Source
import scala.collection.mutable.{Set, HashMap}
import scala.util.chaining._
/*
 */
object Day5 {
  case class Rule(
      left: Int,
      right: Int
  )

  def main(args: Array[String]): Unit = {
    val (rules, updates) = parseIn
    val sortRules = rules.map(r => ((r.left, r.right))).toSet

    def sortUpdate(u: List[Int]) =
      u.sortWith { (a, b) => sortRules.contains((a, b)) }

    def isValid(u: List[Int]) =
      sortUpdate(u) == u

    val byvalid = updates.groupBy(isValid)

    // part 1
    byvalid(true)
      .map(centrepage)
      .sum
      .pipe(println)

    // part 2
    byvalid(false)
      .map(sortUpdate)
      .map(centrepage)
      .sum
      .pipe(println)

  }

  def parseIn = {
    val in = Source.stdin.mkString
    val (ruleList, rest): (String, String) =
      Tuple.fromArray(in.split("\n\n", 2)): @unchecked
    val rules = "(.+)\\|(.+)".r
      .findAllMatchIn(ruleList)
      .map { _.subgroups.map(_.toInt) }
      .collect { case l :: r :: _ => Rule(l, r) }
      .toList
    val updates = rest.linesIterator.map {
      _.split(',').map(_.toInt).toList
    }.toList
    (rules, updates)
  }

  def centrepage(l: List[Int]) = l(l.length / 2)
}
