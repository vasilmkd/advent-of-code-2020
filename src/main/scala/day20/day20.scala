package day20

import java.util.regex.{Matcher, Pattern}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.matching.Regex

case class Id(value: Int) extends AnyVal

enum Side:
  case Top, Bottom, Left, Right

case class Edge(value: String, side: Side)

extension (str: String)
  def indexOfRegex(regex: String): Option[Int] =
    val pattern = Pattern.compile(regex)
    val matcher = pattern.matcher(str)
    if matcher.find() then Some(matcher.start()) else None

extension (tile: Array[Array[Char]])
  def transposed: Array[Array[Char]] =
    for
      i <- (0 until tile.length).toArray
    yield tile.map(_(i))

  def flipped: Array[Array[Char]] =
    tile.reverse

  def rotated: Array[Array[Char]] =
    tile.flipped.transposed

  def tileString: String =
    tile.map(_.mkString).mkString("\n")

case class RawTile(id: Id, tile: Array[Array[Char]]):
  def rotated: RawTile =
    RawTile(id, tile.rotated)

  def flipped: RawTile =
    RawTile(id, tile.flipped)

  def top: Edge =
    Edge(tile.head.mkString, Side.Top)

  def bottom: Edge =
    Edge(tile.last.mkString, Side.Bottom)

  def left: Edge =
    Edge(tile.map(_.head).mkString, Side.Left)

  def right: Edge =
    Edge(tile.map(_.last).mkString, Side.Right)

case class ParseState(tiles: Vector[RawTile], current: Option[(Id, Vector[String])])

val tileIdRegex: Regex = """Tile (\d*):""".r

def parseFoldFn(state: ParseState, line: String): ParseState =
  line match
    case tileIdRegex(id) => state.copy(current = Some((Id(id.toInt), Vector.empty)))
    case "" =>
      val (id, lines) = state.current.get
      ParseState(state.tiles :+ RawTile(id, lines.map(_.toCharArray).toArray), None)
    case line => state.copy(current = state.current.map(t => (t._1, t._2 :+ line)))

case class SearchState(x: Int, y: Int, edge: Edge)

case class Tile(x: Int, y: Int, tile: RawTile)

case class Puzzle(tiles: Vector[Tile])

def edges(x: Int, y: Int, tile: Tile): Vector[SearchState] =
  Vector(
    SearchState(x, y + 1, tile.tile.top),
    SearchState(x - 1, y, tile.tile.left),
    SearchState(x, y - 1, tile.tile.bottom),
    SearchState(x + 1, y, tile.tile.right)
  )

def allOrientations(raw: RawTile): Vector[RawTile] =
  Vector(
    raw,
    raw.rotated,
    raw.rotated.rotated,
    raw.rotated.rotated.rotated,
    raw.flipped,
    raw.flipped.rotated,
    raw.flipped.rotated.rotated,
    raw.flipped.rotated.rotated.rotated
  )

def findMatching(edge: Edge, raw: Vector[RawTile]): (Option[RawTile], Vector[RawTile]) =
  edge.side match
    case Side.Top =>
      val found = raw.flatMap(allOrientations(_)).find(_.bottom.value == edge.value)
      (found, found.map(f => raw.filterNot(_.id == f.id)).getOrElse(raw))
    case Side.Bottom =>
      val found = raw.flatMap(allOrientations(_)).find(_.top.value == edge.value)
      (found, found.map(f => raw.filterNot(_.id == f.id)).getOrElse(raw))
    case Side.Left =>
      val found = raw.flatMap(allOrientations(_)).find(_.right.value == edge.value)
      (found, found.map(f => raw.filterNot(_.id == f.id)).getOrElse(raw))
    case Side.Right =>
      val found = raw.flatMap(allOrientations(_)).find(_.left.value == edge.value)
      (found, found.map(f => raw.filterNot(_.id == f.id)).getOrElse(raw))

def solve(start: Tile, raw: Vector[RawTile]): Puzzle =
  @tailrec
  def loop(queue: Queue[SearchState], raw: Vector[RawTile], visited: Set[(Int, Int)], acc: Vector[Tile]): Vector[Tile] =
    queue match
      case SearchState(x, y, edge) +: tail =>
        if visited((x, y)) then loop(tail, raw, visited, acc)
        else
          val (opt, newRaw) = findMatching(edge, raw)
          opt match
            case None =>
              loop(tail, newRaw, visited, acc)
            case Some(r) =>
              val tile = Tile(x, y, r)
              val es = edges(x, y, tile)
              loop(tail.enqueueAll(es), newRaw, visited + (x -> y), acc :+ tile)
      case _ => acc

  val init = edges(0, 0, start)
  Puzzle(loop(Queue.empty.enqueueAll(init), raw.filterNot(_ == start.tile), Set((0, 0)), Vector(start)))

@main
def part1(): Unit =
  val ParseState(raw, _) =
    (Source.fromResource("day20.txt").getLines() ++ List(""))
      .foldLeft(ParseState(Vector.empty, None))(parseFoldFn)
  val result = solve(Tile(0, 0, raw.head), raw.tail)
  val (minX, maxX, minY, maxY) = result.tiles.foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue)):
    case ((minX, maxX, minY, maxY), t) => (math.min(minX, t.x), math.max(maxX, t.x), math.min(minY, t.y), math.max(maxY, t.y))
  val c1 = result.tiles.find(t => t.x == minX && t.y == minY).get
  val c2 = result.tiles.find(t => t.x == maxX && t.y == minY).get
  val c3 = result.tiles.find(t => t.x == minX && t.y == maxY).get
  val c4 = result.tiles.find(t => t.x == maxX && t.y == maxY).get
  val product = Vector(c1, c2, c3, c4).map(_.tile.id.value.toLong).product
  println(product)

def combine(row: Vector[Tile], n: Int): Array[Array[Char]] =
  for
    i <- (0 until row.head.tile.tile.length).toArray
  yield row.map(_.tile.tile(i)).foldLeft(Array.empty[Char])(_ ++ _)

def trim(tile: Array[Array[Char]]): Array[Array[Char]] =
  tile.map(_.tail.init).tail.init

val monster1: String = "                  # ".replace(" ", ".")
val monster2: String = "#    ##    ##    ###".replace(" ", ".")
val monster3: String = " #  #  #  #  #  #   ".replace(" ", ".")

def findMonster(group: Vector[String]): Boolean =
  group(1).tails.map(_.indexOfRegex(monster2)).exists:
    case Some(n) =>
      group(0).drop(n).indexOfRegex(monster1) == Some(0) && group(2).drop(n).indexOfRegex(monster3) == Some(0)
    case None => false

def countMonsters(puzzle: Array[Array[Char]]): Int =
  puzzle.map(_.mkString).toVector.sliding(3).count(findMonster)

def mergePuzzle(puzzle: Puzzle, n: Int): Array[Array[Char]] =
  val trimmed = puzzle.tiles.map(t => t.copy(tile = t.tile.copy(tile = trim(t.tile.tile))))
  val (minX, maxX, minY, maxY) = trimmed.foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue)):
    case ((minX, maxX, minY, maxY), t) => (math.min(minX, t.x), math.max(maxX, t.x), math.min(minY, t.y), math.max(maxY, t.y))
  val groups = trimmed.map(t => t.copy(x = t.x - minX, y = t.y - minY)).sortBy(t => (-t.y, t.x)).grouped(n).toVector
  groups.map(combine(_, n).tileString).foldLeft(Array.empty[Array[Char]])(_ ++ _.split("\n").map(_.toCharArray))

@main
def part2(): Unit =
  val ParseState(raw, _) =
    (Source.fromResource("day20.txt").getLines() ++ List(""))
      .foldLeft(ParseState(Vector.empty, None))(parseFoldFn)
  val n = math.sqrt(raw.size).toInt
  val solution = solve(Tile(0, 0, raw.head), raw.tail)
  val puzzle = mergePuzzle(solution, n)
  val blah = puzzle.rotated.map(_.mkString).toVector.sliding(3).toVector(17)

  val (correct, monsters) =
    Vector(
      puzzle,
      puzzle.rotated,
      puzzle.rotated.rotated,
      puzzle.rotated.rotated.rotated,
      puzzle.flipped,
      puzzle.flipped.rotated,
      puzzle.flipped.rotated.rotated,
      puzzle.flipped.rotated.rotated.rotated,
    )
    .map(p => (p, countMonsters(p)))
    .find(_._2 > 0)
    .get

  val res = correct.tileString.count(_ == '#')
  println(res - monsters * 15)
