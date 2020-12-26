package day25

import scala.annotation.tailrec
import scala.io.Source

def transform(subject: Long, loopSize: Long): Long =
  @tailrec
  def loop(acc: Long, iterations: Long): Long =
    iterations match
      case 0 => acc
      case n => loop((acc * subject) % 20201227L, n - 1)

  loop(1, loopSize)

def crack(keys: Set[Long]): (Long, Long) =
  @tailrec
  def loop(loopSize: Long, key: Long): (Long, Long) =
    if keys(key) then (loopSize, key)
    else loop(loopSize + 1, (key * 7L) % 20201227L)

  loop(0L, 1L)

@main
def part1(): Unit =
  val keys = Source.fromResource("day25.txt").getLines().map(_.toLong).toVector
  val (loopSize, key) = crack(keys.toSet)
  val other = keys.find(_ != key).get
  val encryptionKey = transform(other, loopSize)
  println(encryptionKey)
