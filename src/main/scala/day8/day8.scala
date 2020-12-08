package day8

import scala.annotation.tailrec
import scala.io.Source

enum Instruction:
  case Nop(delta: Int)
  case Acc(delta: Int)
  case Jmp(delta: Int)
  case End

def parseInstruction(line: String): Instruction =
  val parts = line.split(" ")
  val delta = parts(1).toInt
  parts(0) match
    case "nop" => Instruction.Nop(delta)
    case "acc" => Instruction.Acc(delta)
    case "jmp" => Instruction.Jmp(delta)

def execute(program: Vector[Instruction]): (Boolean, Int) =
  @tailrec
  def loop(program: Vector[Instruction], pc: Int, acc: Int, visited: Set[Int]): (Boolean, Int) =
    val curr = program(pc)
    if visited(pc) then
      (false, acc)
    else
      curr match
        case Instruction.Nop(_) => loop(program, pc + 1, acc, visited + pc)
        case Instruction.Acc(d) => loop(program, pc + 1, acc + d, visited + pc)
        case Instruction.Jmp(d) => loop(program, pc + d, acc, visited + pc)
        case Instruction.End => (true, acc)

  loop(program, 0, 0, Set.empty)

def modifyProgram(program: Vector[Instruction], index: Int): Option[Vector[Instruction]] =
  program(index) match
    case Instruction.Nop(d) => Some(program.updated(index, Instruction.Jmp(d)))
    case Instruction.Jmp(d) => Some(program.updated(index, Instruction.Nop(d)))
    case _ => None

@main
def part1(): Unit =
  val program =
    Source
      .fromResource("day8.txt")
      .getLines()
      .map(parseInstruction)
      .toVector
  val (_, acc) = execute(program)
  println(acc)

@main
def part2(): Unit =
  val program =
    (Source
      .fromResource("day8.txt")
      .getLines()
      .map(parseInstruction) ++ List(Instruction.End))
      .toVector
  val result =
    LazyList
      .range(0, program.length)
      .map((program, _))
      .map(modifyProgram.tupled)
      .collect:
        case Some(p) => execute(p)
      .find(_._1)
      .map(_._2)
      .getOrElse(-1)
  println(result)
