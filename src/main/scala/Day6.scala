import scala.io.Source
import scala.collection.mutable
import scala.util.chaining._

/*
 */
object Day6 {
  type XY = (Int, Int)
  val dirs = IndexedSeq((0, -1), (1, 0), (0, 1), (-1, 0))
  def rotRight(d: Int) = (d + 1) % dirs.length

  extension (c: XY) def moveBy(d: XY): XY = (c(0) + d(0), c(1) + d(1))
  val OUT = '_'
  def main(args: Array[String]): Unit = {
    val mat = parseMatrix
    println(part1(mat))
    println(part2(mat))
  }

  def parseMatrix: Map[XY, Char] = {
    Source.stdin.mkString.linesIterator.zipWithIndex
      .flatMap { (row, y) =>
        row.zipWithIndex.map { (c, x) => ((x, y), c) }
      }
      .pipe(Map.from)
      .withDefaultValue(OUT)
  }

  def part1(mat: Map[XY, Char]) = {
    val start = mat.collectFirst { case (coord, '^') => coord }.head
    def visitedPos(currentDir: Int, pos: XY, visited: Set[XY]): Set[XY] = {
      val at = mat(pos)
      if (at == OUT) visited // end
      else {
        val nextPos = pos.moveBy(dirs(currentDir))
        if (mat(nextPos) == '#')
          visitedPos(rotRight(currentDir), pos, visited + pos)
        else visitedPos(currentDir, nextPos, visited + pos)
      }
    }
    visitedPos(0, start, Set()).size
  }

  def part2(mat: Map[XY, Char]) = {
    val start = mat.collectFirst { case (coord, '^') => coord }.head
    var count = 0
    def isALoop(
        currentDir: Int,
        obstaclePos: Option[XY],
        pos: XY,
        visitedWithDir: Set[(XY, Int)],
        foundLoopPos: Set[XY]
    ): Set[XY] = {
      if (mat(pos) == OUT) return Set()
      if (visitedWithDir.contains((pos, currentDir))) {
        count += 1
        // if (count == 1400) // HACK
        //   {
        //     println()
        //     print(mat, start, obstaclePos, visitedWithDir)
        //   }
        // we are in a loop rn, add the obstacle.
        return obstaclePos.toSet
      }
      val nextPos = pos.moveBy(dirs(currentDir))
      if (obstaclePos.contains(nextPos) || mat(nextPos) == '#') // continue path
        return isALoop(
          rotRight(currentDir),
          obstaclePos,
          pos,
          visitedWithDir + ((pos, currentDir)),
          foundLoopPos
        )

      val sameObstacle = isALoop(
        currentDir,
        obstaclePos,
        nextPos,
        visitedWithDir + ((pos, currentDir)),
        foundLoopPos
      )
      // Try putting an obstacle at nextPos if we are not already/have not
      // checked this It is not allowed if we have already visited the space, as
      // this obstacle position would already have been checked!
      if (
        obstaclePos.isDefined
        || mat(nextPos) != '.'
        || foundLoopPos.contains(nextPos)
        || dirs.indices.exists { d =>
          visitedWithDir.contains((nextPos, d)) /// GGGGGGGGGRRRRGG
        }
      ) return sameObstacle
      return sameObstacle ++ isALoop(
        currentDir, // don't turn, yet
        Some(nextPos),
        pos, // don't move
        visitedWithDir, // don't visit!
        foundLoopPos
      )
    }
    isALoop(0, None, start, Set(), Set()).size
  }

  def print(
      mat: Map[XY, Char],
      start: XY,
      obstaclePos: Option[XY],
      visitedWithDir: Set[(XY, Int)]
  ) = {
    val maxX = mat.keys.map(_._1).max
    val maxY = mat.keys.map(_._2).max
    val vis = visitedWithDir.contains
    for (y <- (0 to maxY))
      (0 to maxX)
        .map { (x) =>
          val c = (x, y)
          if (obstaclePos.contains(c)) "O"
          else if (c == start) {
            assert(vis(c, 0))
            "^"
          } else {
            val h = vis(c, 1) || vis(c, 3)
            val v = vis(c, 0) || vis(c, 2)
            if (h && v) "+"
            else if (h) "-"
            else if (v) "|"
            else mat(c)
          }
        }
        .mkString
        .pipe(println)
  }
}
