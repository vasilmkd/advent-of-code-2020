package day24

import scala.annotation.tailrec
import scala.io.Source

type Delta = -1 | 0 | 1

enum Tile:
  case White, Black

  def unary_! : Tile =
    this match
      case White => Black
      case Black => White

enum Direction(val dx: Delta, val dy: Delta, val dz: Delta):
  case E  extends Direction(1, -1, 0)
  case W  extends Direction(-1, 1, 0)
  case SE extends Direction(0, -1, 1)
  case SW extends Direction(-1, 0, 1)
  case NE extends Direction(1, 0, -1)
  case NW extends Direction(0, 1, -1)

object Direction:
  def unapply(direction: Direction): Option[(Delta, Delta, Delta)] =
    Some((direction.dx, direction.dy, direction.dz))

case class Coordinate(x: Long, y: Long, z: Long):
  def move(direction: Direction): Coordinate =
    val Direction(dx, dy, dz) = direction
    Coordinate(x + dx, y + dy, z + dz)

type Universe = Map[Coordinate, Tile.Black.type]

def parseLine(line: String): Vector[Direction] =
  @tailrec
  def loop(line: String, acc: Vector[Direction]): Vector[Direction] =
    if line.isEmpty then acc
    else
      val (h, t) = (line.head, line.tail)
      h match
        case 'e' => loop(t, acc :+ Direction.E)
        case 'w' => loop(t, acc :+ Direction.W)
        case 'n' =>
          val (hd, tl) = (t.head, t.tail)
          hd match
            case 'e' => loop(tl, acc :+ Direction.NE)
            case 'w' => loop(tl, acc :+ Direction.NW)
        case 's' =>
          val (hd, tl) = (t.head, t.tail)
          hd match
            case 'e' => loop(tl, acc :+ Direction.SE)
            case 'w' => loop(tl, acc :+ Direction.SW)

  loop(line, Vector.empty)

val zero = Coordinate(0L, 0L, 0L)

def adjacent(coordinate: Coordinate): Vector[Coordinate] =
  Direction.values.map(coordinate.move).toVector

def rule1(universe: Universe)(coordinate: Coordinate): Boolean =
  lazy val count = adjacent(coordinate).count(universe.contains)
  universe(coordinate) == Tile.Black && (count == 0 || count > 2)

def rule2(universe: Universe)(coordinate: Coordinate): Set[Coordinate] =
  adjacent(coordinate)
    .filter(!universe.contains(_))
    .filter(adjacent(_).count(universe.contains) == 2)
    .toSet

def iteration(universe: Universe): Universe =
  val r1 = universe.filter(t => !rule1(universe)(t._1)).keySet
  val r2 = universe.foldLeft(Set.empty[Coordinate]):
    case (acc, (c, _)) => acc union rule2(universe)(c)
  (r1 union r2).map((_, Tile.Black).asInstanceOf[(Coordinate, Tile.Black.type)]).toMap

@main
def part1(): Unit =
  val lines = Source.fromResource("day24.txt").getLines().map(parseLine).toVector
  val result =
    lines
      .foldLeft(Map.empty[Coordinate, Tile.Black.type]):
        (universe, line) =>
          universe.updatedWith(line.foldLeft(zero)(_ move _)):
            case None => Some(Tile.Black)
            case _ => None
  println(result.size)

@main
def part2(): Unit =
  val lines = Source.fromResource("day24.txt").getLines().map(parseLine).toVector
  val universe =
    lines
      .foldLeft(Map.empty[Coordinate, Tile.Black.type]):
        (universe, line) =>
          universe.updatedWith(line.foldLeft(zero)(_ move _)):
            case None => Some(Tile.Black)
            case _ => None
  val result =
    LazyList
      .iterate(iteration(universe))(iteration)
      .zipWithIndex
      .takeWhile(_._2 < 100)
      .last
      ._1
  println(result.size)
