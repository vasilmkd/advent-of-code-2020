package day12

import scala.io.Source

enum Instruction:
  case N(delta: Int)
  case S(delta: Int)
  case E(delta: Int)
  case W(delta: Int)
  case L(degrees: Int)
  case R(degrees: Int)
  case F(delta: Int)

def parseInstruction(line: String): Instruction =
  val char = line.charAt(0)
  val value = line.drop(1).toInt
  char match
    case 'N' => Instruction.N(value)
    case 'S' => Instruction.S(value)
    case 'E' => Instruction.E(value)
    case 'W' => Instruction.W(value)
    case 'L' => Instruction.L(value)
    case 'R' => Instruction.R(value)
    case 'F' => Instruction.F(value)

case class State1(degrees: Int, x: Int, y: Int)

def moduloCircle(value: Int): Int =
  if (value < 0) 360 + value
  else if (value >= 360) value - 360
  else value

def foldFn1(state: State1, instruction: Instruction): State1 =
  instruction match
    case Instruction.N(delta) => state.copy(y = state.y + delta)
    case Instruction.S(delta) => state.copy(y = state.y - delta)
    case Instruction.E(delta) => state.copy(x = state.x + delta)
    case Instruction.W(delta) => state.copy(x = state.x - delta)
    case Instruction.L(deg) => state.copy(degrees = moduloCircle(state.degrees - deg))
    case Instruction.R(deg) => state.copy(degrees = moduloCircle(state.degrees + deg))
    case Instruction.F(delta) =>
      state.degrees match
        case 0 => state.copy(y = state.y + delta)
        case 90 => state.copy(x = state.x + delta)
        case 180 => state.copy(y = state.y - delta)
        case 270 => state.copy(x = state.x - delta)

case class State2(wayX: Int, wayY: Int, x: Int, y: Int)

def foldFn2(state: State2, instruction: Instruction): State2 =
  instruction match
    case Instruction.N(delta) => state.copy(wayY = state.wayY + delta)
    case Instruction.S(delta) => state.copy(wayY = state.wayY - delta)
    case Instruction.E(delta) => state.copy(wayX = state.wayX + delta)
    case Instruction.W(delta) => state.copy(wayX = state.wayX - delta)
    case Instruction.L(degrees) =>
      degrees match
        case 90 => state.copy(wayX = -state.wayY, wayY = state.wayX)
        case 180 => state.copy(wayX = -state.wayX, wayY = -state.wayY)
        case 270 => state.copy(wayX = state.wayY, wayY = -state.wayX)
    case Instruction.R(degrees) =>
      degrees match
        case 90 => state.copy(wayX = state.wayY, wayY = -state.wayX)
        case 180 => state.copy(wayX = -state.wayX, wayY = -state.wayY)
        case 270 => state.copy(wayX = -state.wayY, wayY = state.wayX)
    case Instruction.F(times) =>
      state.copy(
        y = state.y + times * state.wayY,
        x = state.x + times * state.wayX
      )

@main
def part1(): Unit =
  val result =
    Source
      .fromResource("day12.txt")
      .getLines()
      .map(parseInstruction)
      .foldLeft(State1(90, 0, 0))(foldFn1)
  val manhattan = math.abs(result.x) + math.abs(result.y)
  println(manhattan)

@main
def part2(): Unit =
  val result =
    Source
      .fromResource("day12.txt")
      .getLines()
      .map(parseInstruction)
      .foldLeft(State2(10, 1, 0, 0))(foldFn2)
  val manhattan = math.abs(result.x) + math.abs(result.y)
  println(manhattan)
