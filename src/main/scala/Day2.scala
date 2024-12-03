import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map, HashMap}

object Day2 {

  def main(args: Array[String]): Unit = {
    val reports = ListBuffer[List[Int]]()
    for (line <- Source.stdin.getLines) {
      val row = line.split(" +")
      if (row.size > 1)
        reports += row.map(Integer.parseInt).toList
    }
    println(countSafe2(reports.toList))
  }

  // part 1
  def countSafe(reports: List[List[Int]]): Int = {
    def isSafe(report: List[Int]): Boolean = {
      val differences = report.zip(report.tail).map((a, b) => (a - b))
      val sign = differences.head.sign;
      !differences.exists(d => {
        d.sign != sign || d.abs < 1 || d.abs > 3
      })
    }
    reports.count(isSafe)
  }

  // part 2
  def countSafe2(reports: List[List[Int]]): Int = {
    def isSafe(
        report: List[Int],
        previousValue: Option[Int] = None,
        sign: Int = 0,
        ignoreUsed: Boolean = false
    ): Boolean = {
      if (report.isEmpty) true
      else {
        // On the first entry in the report, we are safe
        val (headSafe, nextSign) = previousValue.fold((true, sign))(p => {
          val d = report.head - p
          // Otherwise it depends on matching sign and diff
          (1 <= d.abs && d.abs <= 3 && (sign == 0 || d.sign == sign), d.sign)
        })
        // Recurse without ignoring report.head
        (
          headSafe && isSafe(
            report.tail,
            Some(report.head),
            nextSign,
            ignoreUsed
          )
        ) || (
          // Recurse while ignoring report.head if we can
          !ignoreUsed && isSafe(report.tail, previousValue, sign, true)
        )
      }
    }
    reports.count((r) => isSafe(r))
  }
}
