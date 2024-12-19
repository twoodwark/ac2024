import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
/*
 */
@main def Day19 =

  def parse =
    val Array(parts, patterns) = Source.stdin.mkString.split("\n\n")
    (parts.strip.split(", ").toSet, patterns.strip.split("\n").toSeq)

  val (parts, patterns) = parse

  def dfs_memoized[V, T](
      terminate: PartialFunction[V, T],
      step: V => Iterable[V],
      reduce: Iterable[T] => T
  ): V => T =
    val cache = mutable.Map[V, T]()
    lazy val search: V => T =
      terminate.orElse: x =>
        cache.getOrElseUpdate(
          x,
          reduce:
            for n <- step(x)
            yield search(n)
        )

    search

  def searchStep(pat: String) = for
    p <- parts.view // avoid returning a set
    if pat.startsWith(p)
  yield pat.drop(p.size)

  def part1 =
    val get = dfs_memoized[String, Boolean](
      { case x if x.isEmpty => true },
      searchStep,
      _.exists(identity)
    )
    patterns.count(get)

  def part2 =
    val get = dfs_memoized[String, Long](
      { case x if x.isEmpty => 1L },
      searchStep,
      _.sum.toLong
    )
    patterns.map(get).sum

  part1.pipe(println)
  part2.pipe(println)
