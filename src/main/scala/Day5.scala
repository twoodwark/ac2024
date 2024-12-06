import scala.io.Source
import scala.collection.mutable.{Set, HashMap}

/*
 */
object Day5 {
  case class Rule(
      left: Int,
      right: Int
  )
  def main(args: Array[String]): Unit = {
    println(part1.tupled(parseIn))
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
  def part1(rules: List[Rule], updates: List[List[Int]]) = {
    // key rules both ways
    val rulesByLeft = new HashMap[Int, Set[Rule]].withDefault(_ => Set.empty)
    val rulesByRight = new HashMap[Int, Set[Rule]].withDefault(_ => Set.empty)
    for (r <- rules) {
      rulesByLeft.getOrElseUpdate(r.left, Set.empty).add(r)
      rulesByRight.getOrElseUpdate(r.right, Set.empty).add(r)
    }

    def isValid(u: List[Int]) = {
      val pendingRules = Set[Rule]()
      u.foreach { page =>
        // find any rightrules that reference this number
        // we can remove them from the pending list
        pendingRules.subtractAll(rulesByRight(page))
        // add any leftrules that reference this number
        pendingRules.addAll(rulesByLeft(page))
      }
      val pages = u.toSet
      // Valid unless pending rules reference present pages
      !pendingRules.exists { r =>
        pages.contains(r.right) && pages.contains(r.left)
      }
    }

    updates
      .filter(isValid)
      .map(centrepage)
      .sum
  }
}
