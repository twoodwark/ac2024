import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map, HashMap}

@main def hello(): Unit =
  val left = ListBuffer[Int]()
  val right = ListBuffer[Int]()
  for (line <- Source.stdin.getLines) {
    val row = line.split(' ')
    left += Integer.parseInt(row.head)
    right += Integer.parseInt(row.last)
  }
  println(similarity(left.toList, right.toList))

  // part 1
def countTotalDist(left: List[Int], right: List[Int]): Int = {
  val dists = left.sorted.zip(right.sorted).map((a, b) => (a - b).abs)
  dists.sum
}

def similarity(left: List[Int], right: List[Int]): Int = {
  val rightSet = Map.WithDefault(new HashMap[Int, Int], _ => 0)
  for (r <- right) rightSet(r) += 1
  left.iterator.map(l => rightSet.get(l).getOrElse(0) * l).sum
}
