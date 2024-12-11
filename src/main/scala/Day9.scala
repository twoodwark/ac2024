import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
import scala.annotation.tailrec
/*

 */
object Day9 extends App {
  val source = Source.stdin.mkString
  case class File(id: Int, length: Int)
  type FileOrSpace = Either[File, Int]
  extension (f: FileOrSpace) def isSpace = f.isRight
  extension (f: FileOrSpace)
    def length = f match
      case Left(value)  => value.length
      case Right(value) => value

  case class Disk(
      compacted: mutable.ArrayDeque[File],
      sparse: mutable.ArrayDeque[FileOrSpace]
  ) {
    def removeTrailingSpace() = {
      val spaces = sparse.reverseIterator.takeWhile(_.isSpace).size
      sparse.dropRightInPlace(spaces)
    }
    def contents: Iterator[FileOrSpace] =
      compacted.iterator.map(Left(_)) ++ sparse.iterator
    def contentsOffsets =
      contents.scanLeft(0)(_ + _.length)
    def checksum: Long = {
      for case (Left(File(id, length)), offset) <- contents.zip(
          contentsOffsets
        )
      yield {
        // println(("offset", offset, id.toLong, length))
        id.toLong * { offset * length + (length * (length - 1)) / 2 }
      }
    }.sum
  }
  def compact1(disk: Disk) =
    def consumeSpaceAndReturnRemaining(
        spaceAmount: Int,
        f: File
    ): (Int, File, File) =
      if spaceAmount >= f.length then (spaceAmount - f.length, f, File(f.id, 0))
      else (0, File(f.id, spaceAmount), File(f.id, f.length - spaceAmount))

    disk.removeTrailingSpace()
    // is doing this recursively stupid?
    while disk.sparse.nonEmpty do
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

  def parseDisk = {
    var fileIndex = -1
    val contents =
      for
        (n, idx) <- source.toList.zipWithIndex
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
      mutable.ArrayDeque.from(contents.tail)
    )
  }

  def parseDisk2 = {
    var fileIndex = -1
    var offset = 0
    val contents =
      for
        (n, idx) <- source.toList.zipWithIndex
        if n != '\n'
      yield {
        val length = n.toString().toInt
        offset += length
        if idx % 2 == 0
        then Left(offset - length, File({ fileIndex += 1; fileIndex }, length))
        else Right(offset - length, length)
      }
    // Hmmm: use .partitionMap!
    // Ordered by offset
    val files = mutable.TreeMap.from(contents.collect { case Left(value) =>
      value
    })
    // Ordered by offset
    val spaces = mutable.TreeMap.from(contents.collect { case Right(value) =>
      value
    })
    (files, spaces)
  }
  extension (
      t: (
          mutable.TreeMap[Int, File], // files by index
          mutable.TreeMap[Int, Int] // spaces by index
      )
  )
    def print_() = {
      var lastOff = 0
      for (offset, File(id, length)) <- t._1
      do
        (lastOff to offset - 1).foreach { (_) => print('.') }
        (0 to length - 1).foreach { (_) => print(id) }
        lastOff = offset + length
      println()
      lastOff = 0
      for (offset, length) <- t._2
      do
        (lastOff to offset - 1).foreach { (_) => print(' ') }
        (0 to length - 1).foreach { (_) => print("_") }
        lastOff = offset + length
      println()
    }
    def checksum: Long = {
      for (offset, File(id, length)) <- t._1
      yield id.toLong * { offset * length + (length * (length - 1)) / 2 }
    }.sum
    def compact() = {
      for (fileOffs, f) <- t._1.toArray.reverseIterator
      do
        // remove all spaces at higher offsets
        t._2 --= t._2.rangeFrom(fileOffs).keys.toList

        t._2.find((spaceOffs, spaceSize) => spaceSize >= f.length) foreach {
          (spaceOffs, spaceSize) =>
            assert(fileOffs > spaceOffs)
            assert(!t._1.contains(spaceOffs))
            t._2 -= spaceOffs
            // move file
            // println(("move", f.id, "from", offset, "to", spaceOffs))
            t._1 -= fileOffs
            t._1 += spaceOffs -> f
            if f.length < spaceSize then
              // add new space
              t._2 += (spaceOffs + f.length) -> (spaceSize - f.length)
        }
    }

  // part 1
  val disk1 = parseDisk
  compact1(disk1)
  disk1.checksum.pipe(println)
  // part 2
  val disk2 = parseDisk2
  disk2.compact()
  disk2.checksum.pipe(println)
}
