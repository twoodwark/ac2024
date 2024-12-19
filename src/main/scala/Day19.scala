import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
import scala.collection.immutable
import scala.compiletime.ops.boolean
/*
 */
@main def Day19 =

  def parse =
    val Array(parts, patterns) = Source.stdin.mkString.split("\n\n")
    (parts.strip.split(", ").toSet, patterns.strip.split("\n").toSeq)

  val (parts, patterns) = parse

  def part1 =
    // recursive dfs
    val cache = mutable.Map[String, Boolean]()
    def search(pat: String): Boolean =
      pat.isEmpty() || cache.getOrElseUpdate(
        pat, {
          val found = for
            p <- parts
            if pat.startsWith(p)
          yield search(pat.drop(p.size))
          found.find(identity).getOrElse(false)
        }
      )

    patterns.count(search)

  part1.pipe(println)
