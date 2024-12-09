import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
import scala.annotation.tailrec
/*

 */
object Day9 extends App {
// file id/length
  case class File(id: Int, length: Int)
  type FileOrSpace = Either[File, Int]
  extension (f: FileOrSpace) def isSpace = f.isRight
  extension (f: FileOrSpace)
    def length = f match
      case Left(value)  => value.length
      case Right(value) => value

  case class Disk(
      compacted: mutable.ArrayDeque[File],
      sparse: mutable.ArrayDeque[FileOrSpace],
      size: Int
  ) {
    def removeTrailingSpace() = {
      val spaces = sparse.reverseIterator.takeWhile(_.isSpace).size
      sparse.dropRightInPlace(spaces)
    }
    def contents: Iterable[FileOrSpace] =
      compacted.toIterable.map(Left(_)) ++ sparse.toIterable
    def contentsOffsets =
      contents.view.scanLeft(0)(_ + _.length)
    def checksum: Long = {
      for case (Left(File(id, length)), offset) <- contents.view.zip(
          contentsOffsets
        )
      yield {
        // println(("offset", offset, id.toLong, length))
        id.toLong * { offset * length + (length * (length - 1)) / 2 }
      }
    }.sum
  }
  def compact(disk: Disk) =
    def consumeSpaceAndReturnRemaining(
        spaceAmount: Int,
        f: File
    ): (Int, File, File) =
      if spaceAmount >= f._2 then (spaceAmount - f.length, f, File(f.id, 0))
      else (0, File(f.id, spaceAmount), File(f.id, f.length - spaceAmount))

    disk.removeTrailingSpace()
    // is doing this recursively stupid?
    while (disk.sparse.nonEmpty) {
      val (availSpace, f) =
        (disk.sparse.removeHead(), disk.sparse.removeLast()) match
          case (Right(space), Left(f)) => (space, f)
          case _                       => throw Exception("whoops")
      // consume the first space using last file
      val (spaceRemaining, fileCompacted, fileSparse) =
        consumeSpaceAndReturnRemaining(availSpace, f)
      disk.compacted.append(fileCompacted)
      if fileSparse.length > 0 then disk.sparse.append(Left(fileSparse))
      else disk.removeTrailingSpace()
      if spaceRemaining > 0 then disk.sparse.prepend(Right(spaceRemaining))
      else
        // move the adjacent file
        disk.sparse.removeHead().left.map(disk.compacted.append)
    }

  def parseDisk = {
    var fileIndex = -1
    val contents =
      for
        (n, idx) <- Source.stdin.toList.zipWithIndex
        if n != '\n'
      yield {
        val length = n.toString().toInt
        if idx % 2 == 0 then Left(File({ fileIndex += 1; fileIndex }, length))
        else Right(length)
      }
    val first = contents.head.left.toSeq
    Disk(
      // always one file
      mutable.ArrayDeque(first*),
      // always starts with space
      mutable.ArrayDeque.from(contents.tail),
      contents.map(_.length).sum
    )
  }
  val disk = parseDisk
  // part 1
  compact(disk)
  disk.checksum.pipe(println)
}
