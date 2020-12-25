package day22

import scala.annotation.tailrec
import scala.io.Source

case class ParseState(p1: Vector[Int], marker: Boolean, p2: Vector[Int])

def parseLine(state: ParseState, line: String): ParseState =
  line match
    case "Player 1:" | "Player 2:" => state
    case "" => state.copy(marker = true)
    case line if state.marker => state.copy(p2 = state.p2 :+ line.toInt)
    case line => state.copy(p1 = state.p1 :+ line.toInt)

@tailrec
def game(p1: Vector[Int], p2: Vector[Int]): Vector[Int] =
  (p1, p2) match
    case (h1 +: t1, h2 +: t2) if h1 > h2 => game(t1 ++ Vector(h1, h2), t2)
    case (h1 +: t1, h2 +: t2) => game(t1, t2 ++ Vector(h2, h1))
    case (Vector(), p2) => p2
    case (p1, _) => p1

def recursive(p1: Vector[Int], p2: Vector[Int]): (Vector[Int], 1 | 2) =
  @tailrec
  def loop(p1: Vector[Int], p2: Vector[Int], seen: Set[Vector[Int]]): (Vector[Int], 1 | 2) =
    if seen(p1) then (p1, 1)
    else
      (p1, p2) match
        case (h1 +: t1, h2 +: t2) if t1.sizeIs >= h1 && t2.sizeIs >= h2 =>
          recursive(t1.take(h1), t2.take(h2)) match
            case (cards, 1) => loop(t1 ++ Vector(h1, h2), t2, seen + p1) 
            case (cards, _) => loop(t1, t2 ++ Vector(h2, h1), seen + p1)
        case (h1 +: t1, h2 +: t2) if h1 > h2 => loop(t1 ++ Vector(h1, h2), t2, seen + p1)
        case (h1 +: t1, h2 +: t2) => loop(t1, t2 ++ Vector(h2, h1), seen + p1)
        case (Vector(), p2) => (p2, 2)
        case (p1, _) => (p1, 1)

  loop(p1, p2, Set.empty)

def calculateScore(deck: Vector[Int]): Int =
  deck.reverse.zipWithIndex.map((c, i) => c * (i + 1)).sum

@main
def part1(): Unit =
  val ParseState(p1, _, p2) =
    Source
      .fromResource("day22.txt")
      .getLines()
      .foldLeft(ParseState(Vector.empty, false, Vector.empty))(parseLine)
  val result = calculateScore(game(p1, p2))
  println(result)

@main
def part2(): Unit =
  val ParseState(p1, _, p2) =
    Source
      .fromResource("day22.txt")
      .getLines()
      .foldLeft(ParseState(Vector.empty, false, Vector.empty))(parseLine)
  val result = calculateScore(recursive(p1, p2)._1)
  println(result)
