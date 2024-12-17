import scala.io.Source
import scala.util.chaining._
import scala.collection.mutable
import scala.collection.immutable
/*
 */
object Day17 extends App {
  case class State(
      a: Long,
      b: Long,
      c: Long,
      instructionPointer: Int = 0,
      output: Seq[Int] = Seq()
  ) {
    def out(v: Int) = copy(output = output :+ v)
  }
  type Program = IndexedSeq[Int]
  type Op = (State, Long) => State
  type Operand = (State, Int) => Long
  case class Instruction(name: String, op: Op, arg: Operand)
  def literalOperand = (s: State, i: Int) => i.toLong
  def comboOperand = (s: State, i: Int) =>
    i match
      case 4 => s.a
      case 5 => s.b
      case 6 => s.c
      case 7 => throw Exception("?")
      case _ => i.toLong
  val INSTRUCTIONS = IndexedSeq(
    Instruction(
      "adv",
      (s, i) => s.copy(a = (s.a.toDouble / math.pow(2, i.toDouble)).toLong),
      comboOperand
    ),
    Instruction("bxl", (s, i) => s.copy(b = s.b ^ i), literalOperand),
    Instruction("bst", (s, i) => s.copy(b = i % 8), comboOperand),
    Instruction(
      "jnz",
      (s, i) =>
        if s.a == 0 then s
        else s.copy(instructionPointer = i.toInt - 2),
      literalOperand
    ),
    Instruction("bxc", (s, _) => s.copy(b = s.b ^ s.c), literalOperand),
    Instruction("out", (s, i) => s.out((i % 8).toInt), comboOperand),
    Instruction(
      "bdv",
      (s, i) => s.copy(b = (s.a.toDouble / math.pow(2, i.toDouble)).toLong),
      comboOperand
    ),
    Instruction(
      "cdv",
      (s, i) => s.copy(c = (s.a.toDouble / math.pow(2, i.toDouble)).toLong),
      comboOperand
    )
  )

  val (state, program) = parse

  part1.pipe(println)

  def part1 =
    val (finalState, _) = Iterator
      .iterate((state, false)) { (s, _) =>
        val ptr = s.instructionPointer
        if ptr >= program.length then (s, true)
        else
          val code = program(ptr)
          val i = program(ptr + 1)
          val Instruction(name, op, arg) = INSTRUCTIONS(code)
          val operandVal = arg(s, i)
          // println(s"$s  ||  @$ptr $name $operandVal")
          val nextS = op(s, operandVal)
          (nextS.copy(instructionPointer = nextS.instructionPointer + 2), false)
      }
      .find(_._2)
      .get
    finalState.output.mkString(",")

  def parse =
    val Array(s, in) = Source.stdin.mkString.split("\n\n")
    val state = s.strip match
      case s"Register A: $a\nRegister B: $b\nRegister C: $c" =>
        State(a = a.toLong, b = b.toLong, c = c.toLong)
    val program = in.split(": ").last.strip.split(",").map(_.toInt)
    (state, program)
}
