package day10

import scala.collection.mutable.Map
import scala.io.Source

case class State(ones: Long, threes: Long)

def foldFn(state: State, pair: Vector[Long]): State =
  val Vector(a, b) = pair
  math.abs(a - b) match
    case 1 => state.copy(ones = state.ones + 1)
    case 3 => state.copy(threes = state.threes + 1)
    case _ => state

val cache: Map[Long, Long] = Map.empty

def possibilities(available: Set[Long])(adapter: Long): Long =
  cache
    .get(adapter)
    .fold:
      val rec = Vector(1, 2, 3).map(_ + adapter).filter(available).map(possibilities(available)).sum
      val res = math.max(rec, 1L)
      cache += (adapter -> res)
      res
    (identity)

@main
def part1(): Unit =
  val adapters = Source.fromResource("day10.txt").getLines().map(_.toLong).toVector.sorted
  val chain = 0L +: adapters :+ (adapters.last + 3L)
  val fold =
    chain
      .sliding(2)
      .foldLeft(State(0L, 0L))(foldFn)
  val result = fold.ones * fold.threes
  println(result)

@main
def part2(): Unit =
  val adapters = Source.fromResource("day10.txt").getLines().map(_.toLong).toVector.sorted
  val available = adapters.toSet
  val result = possibilities(available)(0)
  println(result)
