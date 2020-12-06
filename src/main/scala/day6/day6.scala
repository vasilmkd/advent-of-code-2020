package day6

import scala.io.Source

val Universe = "abcdefghijklmnopqrstuvwxyz".toSet

trait Monoid[A]:
  def identity: A
  extension (x: A):
    def combine(y: A): A
    final def |+| (y: A): A = combine(y)

case class State(sum: Int, partial: Set[Char])

def foldFn(using monoid: Monoid[Set[Char]])(state: State, line: String): State =
  if line.isEmpty then
    val count = state.partial.size
    State(state.sum + count, monoid.identity)
  else
    State(state.sum, state.partial |+| line.toSet)

@main
def part1(): Unit =
  given Monoid[Set[Char]]:
    def identity: Set[Char] = Set.empty
    extension (x: Set[Char]) def combine(y: Set[Char]): Set[Char] =
      x.union(y)

  val result =
    (Source.fromResource("day6.txt").getLines() ++ List(""))
      .foldLeft(State(0, summon[Monoid[Set[Char]]].identity))(foldFn)
      .sum
  println(result)

@main
def part2(): Unit =
  given Monoid[Set[Char]]:
    def identity: Set[Char] = Universe
    extension (x: Set[Char]) def combine(y: Set[Char]): Set[Char] =
      x.intersect(y)

  val result =
    (Source.fromResource("day6.txt").getLines() ++ List(""))
      .foldLeft(State(0, summon[Monoid[Set[Char]]].identity))(foldFn)
      .sum
  println(result)
