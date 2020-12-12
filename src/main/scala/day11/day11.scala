package day11

import scala.io.Source

val directions = Vector(
  (-1, -1),
  (-1, 0),
  (-1, 1),
  (0, -1),
  (0, 1),
  (1, -1),
  (1, 0),
  (1, 1)
)

def inBounds(rows: Int, cols: Int)(row: Int, col: Int): Boolean =
  row >= 0 && row < rows && col >= 0 && col < cols

def addPair(p1: (Int, Int), p2: (Int, Int)): (Int, Int) =
  (p1._1 + p2._1, p1._2 + p2._2)

def adjacent(rows: Int, cols: Int)(row: Int, col: Int): Vector[(Int, Int)] =
  directions
    .map(addPair(_, (row, col)))
    .filter:
      inBounds(rows, cols).tupled

def firstVisible(map: Vector[Vector[Char]])(row: Int, col: Int, direction: (Int, Int)): Option[(Int, Int)] =
  LazyList
    .iterate(direction)(addPair(_, direction))
    .map(addPair(_, (row, col)))
    .takeWhile(inBounds(map.length, map(0).length).tupled)
    .find(visit(map) andThen seat)

def not[A](predicate: A => Boolean): A => Boolean =
  !predicate(_)

def occupied(c: Char): Boolean =
  c == '#'

def seat(c: Char): Boolean =
  c == '#' || c == 'L'

def visit(map: Vector[Vector[Char]])(coord: (Int, Int)) =
  map(coord._1)(coord._2)

def rule1(map: Vector[Vector[Char]])(row: Int, col: Int): Boolean =
  map(row)(col) == 'L'
  && adjacent(map.length, map(0).length)(row, col).map(visit(map)).forall(not(occupied))

def rule2(map: Vector[Vector[Char]])(row: Int, col: Int): Boolean =
  map(row)(col) == '#'
  && adjacent(map.length, map(0).length)(row, col).map(visit(map)).count(occupied) >= 4

def rule1again(map: Vector[Vector[Char]])(row: Int, col: Int): Boolean =
  map(row)(col) == 'L'
  && directions.map(firstVisible(map)(row, col, _)).filter(_.isDefined).forall(_.filter(visit(map) andThen not(occupied)).isDefined)

def rule2again(map: Vector[Vector[Char]])(row: Int, col: Int): Boolean =
  map(row)(col) == '#'
  && directions.map(firstVisible(map)(row, col, _)).filter(_.isDefined).count(_.filter(visit(map) andThen occupied).nonEmpty) >= 5

def check1(map: Vector[Vector[Char]])(row: Int, col: Int): Boolean =
  rule1(map)(row, col) || rule2(map)(row, col)

def check2(map: Vector[Vector[Char]])(row: Int, col: Int): Boolean =
  rule1again(map)(row, col) || rule2again(map)(row, col)

def flip(c: Char): Char =
  c match
    case 'L' => '#'
    case '#' => 'L'
    case c => c

def transform(map: Vector[Vector[Char]], coordinates: Vector[(Int, Int)], check: (Int, Int) => Boolean): Vector[Vector[Char]] =
  val changes = coordinates.filter(check.tupled)
  var copy = map.map(_.toArray).toArray
  changes.foreach:
    (r, c) =>
      copy(r)(c) = flip(map(r)(c))
  copy.map(_.toVector).toVector

def mapString(map: Vector[Vector[Char]]): String =
  map.map(_.mkString).mkString("\n")

def countOccupied(map: Vector[Vector[Char]]): Int =
  map.flatMap(identity).count(occupied)

@main
def part1(): Unit =
  val map = Source.fromResource("day11.txt").getLines().map(_.toVector).toVector
  val coordinates =
    map.zipWithIndex.flatMap:
      (r, i) =>
        r.zipWithIndex.map:
          (_, j) => (i, j)
  var blah = map
  LazyList
    .continually(blah)
    .map(m => transform(m, coordinates, check1(m)))
    .takeWhile(blah != _)
    .tapEach(blah = _)
    .last
  println(countOccupied(blah))

@main
def part2(): Unit =
  val map = Source.fromResource("day11.txt").getLines().map(_.toVector).toVector
  val coordinates =
    map.zipWithIndex.flatMap:
      (r, i) =>
        r.zipWithIndex.map:
          (_, j) => (i, j)
  var blah = map
  LazyList
    .continually(blah)
    .map(m => transform(m, coordinates, check2(m)))
    .takeWhile(blah != _)
    .tapEach(blah = _)
    .last
  println(countOccupied(blah))
