package day23

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

type Circular = mutable.Map[Int, Int]

def game(cups: Circular, start: Int, max: Int, iterations: Int): Circular =
  @tailrec
  def loop(cups: Circular, current: Int, iteration: Int, end: Int): Circular =
    if iteration > end then cups
    else
      val chain1 = cups(current)
      val chain2 = cups(chain1)
      val chain3 = cups(chain2)
      val chain = Set(chain1, chain2, chain3)
      cups += (current -> cups(chain3))
      val destination =
        LazyList
          .iterate(current - 1)(n => if n <= 0 then max else n - 1)
          .dropWhile(n => chain(n) || n <= 0)
          .head
      val oldDestinationNext = cups(destination)
      cups += (destination -> chain1)
      cups += (chain3 -> oldDestinationNext)
      loop(cups, cups(current), iteration + 1, end)

  loop(cups, start, 1, iterations)

@main
def part1(): Unit =
  val input = Source.fromResource("day23.txt").getLines().toVector.head.zipWithIndex.map(_._1.toString.toInt).toVector
  val blah = input :+ input.head
  val cups = input.zipWithIndex.foldLeft(mutable.Map.empty[Int, Int]):
    case (acc, (n, i)) => acc += (n -> blah(i + 1))
  val result = game(cups, input.head, input.max, 100)
  val sequence =
    LazyList
      .iterate(result(1))(result)
      .takeWhile(_ != 1)
      .mkString
  println(sequence)

@main
def part2(): Unit =
  val input = Source.fromResource("day23.txt").getLines().toVector.head.zipWithIndex.map(_._1.toString.toInt).toVector
  var cups = input.zipWithIndex.foldLeft(mutable.Map.empty[Int, Int]):
    case (acc, (n, i)) => acc += (n -> Try(input(i + 1)).getOrElse(10))
  for i <- 10 until 1_000_000
  do cups += (i -> (i + 1))
  cups += (1_000_000 -> input.head)
  val result = game(cups, input.head, 1_000_000, 10_000_000)
  val first = result(1)
  val second = result(first)
  val product = first.toLong * second.toLong
  println(product)
