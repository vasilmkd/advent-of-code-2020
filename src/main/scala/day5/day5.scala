package day5

import scala.annotation.tailrec
import scala.io.Source

enum Half:
  case Upper, Lower

def halfFromLetter(c: Char): Half =
  c match
    case 'F' | 'L' => Half.Lower
    case 'B' | 'R' => Half.Upper

@tailrec
def binarySearch(start: Int, end: Int, sequence: List[Half]): Int =
  sequence match
    case Nil => start
    case hd :: tl =>
      val mid = (start + end) / 2
      hd match
        case Half.Upper => binarySearch(mid, end, tl)
        case Half.Lower => binarySearch(start, mid, tl)

@main
def part1(): Unit =
  val result =
    Source
      .fromResource("day5.txt")
      .getLines()
      .map(_.splitAt(7))
      .map(t => (t._1.toList.map(halfFromLetter), t._2.toList.map(halfFromLetter)))
      .map(t => (binarySearch(0, 128, t._1), binarySearch(0, 8, t._2)))
      .map(t => 8 * t._1 + t._2)
      .max
  println(result)

@main
def part2(): Unit =
  val ids =
    Source
      .fromResource("day5.txt")
      .getLines()
      .map(_.splitAt(7))
      .map(t => (t._1.toList.map(halfFromLetter), t._2.toList.map(halfFromLetter)))
      .map(t => (binarySearch(0, 128, t._1), binarySearch(0, 8, t._2)))
      .map(t => 8 * t._1 + t._2)
      .toVector
      .sorted
  
  val result =
    ids
      .sliding(2)
      .find:
        case Vector(a, b) => b - a == 2
        case _ => false
      .map:
        case Vector(a, _) => a + 1
        case _ => 0
      .getOrElse(0)

  println(result)
