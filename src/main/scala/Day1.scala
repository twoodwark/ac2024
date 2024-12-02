import scala.io.Source
import scala.collection.mutable.ListBuffer

@main def hello(): Unit =
  val left = ListBuffer[Int]()
  val right = ListBuffer[Int]()
  for (line <- Source.stdin.getLines) {

    val row = line.split(' ')
    left.addOne(Integer.parseInt(row.head))
    right.addOne(Integer.parseInt(row.last))
  }
  println(countTotalDist(left.toList, right.toList))

def countTotalDist(left: List[Int], right: List[Int]): Int = {
  val dists = left.sorted.zip(right.sorted).map((a, b) => (a - b).abs)
  dists.sum
}
