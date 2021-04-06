package day14

import scala.io.Source
import scala.util.matching.Regex

enum Instruction:
  case Bitmask(mask: String)
  case Store(address: Long, value: Long)

def pow2(n: Int): Long =
  n match
    case 0 => 1L
    case n =>
      val half = n / 2
      val res = pow2(half) * pow2(half)
      if (n & 1) == 0 then res else 2 * res

def parseBinaryString(str: String): Long =
  str.reverse.replace("X", "1").map(_.toString.toLong).zipWithIndex.map(_ * pow2(_)).sum

def pad(value: Long): String =
  val str = value.toBinaryString
  s"${("0" * (36 - str.length))}$str"

def parseLine(line: String): Instruction =
  if line.startsWith("mask") then parseBitmask(line) else parseStore(line)

def parseBitmask(line: String): Instruction.Bitmask =
  Instruction.Bitmask(line.split(" = ")(1))

val memRegex: Regex = """mem\[(\d*)\]""".r

def parseStore(line: String): Instruction.Store =
  val parts = line.split(" = ")
  parts(0) match
    case memRegex(addr) => Instruction.Store(addr.toLong, parts(1).toLong)

def operation1(l: Char, r: Char): Char =
  (l, r) match
    case (x, 'X') => x
    case (_, x) => x

def calculateValue(value: Long, mask: String): Long =
  parseBinaryString(pad(value).zip(mask).map(operation1).mkString)

case class State(mask: String, memory: Map[Long, Long])

def foldFn1(state: State, instruction: Instruction): State =
  instruction match
    case Instruction.Bitmask(mask) =>
      state.copy(mask = mask)
    case Instruction.Store(addr, value) =>
      state.copy(memory = state.memory.updated(addr, calculateValue(value, state.mask)))

def operation2(l: Char, r: Char): Char =
  (l, r) match
    case (x, '0') => x
    case (_, x) => x

def calculateAddress(address: Long, mask: String): String =
  pad(address).zip(mask).map(operation2).mkString

def expand(mask: String): Vector[String] =
  mask.foldLeft(Vector("")) { (acc, b) =>
    b match
      case 'X' => acc.flatMap(m => Vector(m :+ '0', m :+ '1'))
      case x => acc.map(_ :+ x)
  }

def foldFn2(state: State, instruction: Instruction): State =
  instruction match
    case Instruction.Bitmask(mask) =>
      state.copy(mask = mask)
    case Instruction.Store(addr, value) =>
      state.copy(
        memory =
          expand(calculateAddress(addr, state.mask)).foldLeft(state.memory) { (acc, a) =>
            acc.updated(parseBinaryString(a), value)
          }
      )

@main
def part1(): Unit =
  val result =
    Source
      .fromResource("day14.txt")
      .getLines()
      .map(parseLine)
      .foldLeft(State("0" * 36, Map.empty))(foldFn1)
  println(result.memory.values.sum)

@main
def part2(): Unit =
  val result =
    Source
      .fromResource("day14.txt")
      .getLines()
      .map(parseLine)
      .foldLeft(State("0" * 36, Map.empty))(foldFn2)
  println(result.memory.values.sum)
