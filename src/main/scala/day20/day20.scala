package day20

import scala.io.Source
import scala.util.matching.Regex

case class Tile(id: Long, tile: Array[Array[Char]])

case class ParseState(tiles: Vector[Tile], current: Option[(Long, Vector[String])])

val tileIdRegex: Regex = """Tile (\d*):""".r

def parseFoldFn(state: ParseState, line: String): ParseState =
  line match
    case tileIdRegex(id) => state.copy(current = Some((id.toLong, Vector.empty)))
    case "" =>
      val (id, lines) = state.current.get
      ParseState(state.tiles :+ Tile(id, lines.map(_.toCharArray).toArray), None)
    case line => state.copy(current = state.current.map(t => (t._1, t._2 :+ line)))

def topEdge(tile: Tile): String =
  tile.tile.head.mkString

def bottomEdge(tile: Tile): String =
  tile.tile.last.mkString

def leftEdge(tile: Tile): String =
  tile.tile.map(_.head).mkString

def rightEdge(tile: Tile): String =
  tile.tile.map(_.last).mkString

def allEdges(tile: Tile): Vector[String] =
  Vector(topEdge(tile), bottomEdge(tile), leftEdge(tile), rightEdge(tile))

def checkTile(tiles: Vector[Tile])(tile: Tile): Boolean =
  allEdges(tile).count(!checkEdge(tiles.filterNot(_ == tile))(_)) == 2

def checkEdge(tiles: Vector[Tile])(edge: String): Boolean =
  tiles.map(allEdges).exists(_.map(e => Set(e, e.reverse)).exists(_.contains(edge)))

@main
def part1(): Unit =
  val ParseState(tiles, _) =
    (Source.fromResource("day20.txt").getLines() ++ List(""))
      .foldLeft(ParseState(Vector.empty, None))(parseFoldFn)
  val corners = tiles.filter(checkTile(tiles))
  val result = corners.map(_._1).product
  println(result)
