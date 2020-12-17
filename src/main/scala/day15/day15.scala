package day15

import scala.io.Source

case class State(last: Long, count: Long, memory: Map[Long, Long])

def iteration(state: State): State =
  val State(last, count, memory) = state
  val next = memory.get(last).fold(0L)(count - _)
  State(next, count + 1, memory.updated(last, count))

def program(numbers: Vector[Long], end: Long): Long =
  val starting = numbers.zipWithIndex.map(t => (t._1, t._2.toLong + 1L)).toMap
  val next = numbers.last
  LazyList
    .iterate(State(numbers.last, numbers.size, starting))(iteration)
    .find(_.count == end)
    .get
    .last

@main
def part1(): Unit =
  val numbers = Source.fromResource("day15.txt").getLines().mkString.split(",").map(_.toLong).toVector
  val result = program(numbers, 2020L)
  println(result)

@main
def part2(): Unit =
  val numbers = Source.fromResource("day15.txt").getLines().mkString.split(",").map(_.toLong).toVector
  val result = program(numbers, 30000000L)
  println(result)
