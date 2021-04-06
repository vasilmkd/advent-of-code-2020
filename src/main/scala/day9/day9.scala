package day9

import scala.io.Source

def findInvalidNumber(nums: Vector[Long]): Long =
  LazyList
    .from(nums)
    .sliding(25 + 1)
    .map(_.splitAt(25))
    .map(t => (pairwiseSums(t._1.toVector), t._2.head))
    .find(t => !t._1(t._2))
    .map(_._2)
    .getOrElse(0)

def pairwiseSums(nums: Vector[Long]): Set[Long] =
  nums.combinations(2).map(_.sum).toSet

def contiguous(nums: Vector[Long], index: Int, sum: Long): Option[Long] =
  LazyList
    .from(nums)
    .drop(index)
    .scanLeft((0L, Long.MaxValue, 0L)) {
      case ((s, min, max), n) => (s + n, math.min(min, n), math.max(max, n))
    } 
    .takeWhile(_._1 <= sum)
    .find(_._1 == sum)
    .map(t => t._2 + t._3)

@main
def part1(): Unit =
  val numbers = Source.fromResource("day9.txt").getLines().map(_.toLong).toVector
  val result = findInvalidNumber(numbers)
  println(result)

@main
def part2(): Unit =
  val numbers = Source.fromResource("day9.txt").getLines().map(_.toLong).toVector
  val invalid = findInvalidNumber(numbers)
  val result =
    LazyList
      .from(numbers)
      .zipWithIndex
      .map(t => contiguous(numbers, t._2, invalid))
      .collectFirst {
        case Some(weakness) => weakness
      }
      .getOrElse(-1L)
  println(result)
