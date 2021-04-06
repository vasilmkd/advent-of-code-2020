package day13

import scala.annotation.tailrec
import scala.math.BigInt
import scala.io.Source

def search(bus: BigInt, target: BigInt): BigInt =
  (target % bus) - bus

def extendedEuclid(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) =
  if a == 0 then (b, 0, 1)
  else
    val (g, y, x) = extendedEuclid(b % a, a)
    (g, x - (b / a) * y, y)

def modMul(a: BigInt, b: BigInt, m: BigInt): BigInt =
  (a * b) % m

def solutionPairModuli(a: (BigInt, BigInt), b: (BigInt, BigInt)): (BigInt, BigInt) =
  val (_, x, y) = extendedEuclid(a._1, b._1)
  val mod = a._1 * b._1
  val res = (modMul(modMul(a._2, y, mod), b._1, mod) + modMul(modMul(b._2, x, mod), a._1, mod)) % mod
  (mod, if res < 0 then res + mod else res)

def chineseRemainderTheorem(constraints: Vector[(BigInt, BigInt)]): (BigInt, BigInt) =
  constraints.reduce(solutionPairModuli)

@main
def part1(): Unit =
  val lines = Source.fromResource("day13.txt").getLines().toVector
  val target = BigInt(lines(0).toLong)
  val buses = lines(1).split(",").filterNot(_ == "x").map(_.toLong).map(BigInt(_))
  val result =
    buses
      .map(b => (b, search(b, target)))
      .maxBy(_._2)
  println(result._1 * (-result._2))

@main
def part2(): Unit =
  val lines = Source.fromResource("day13.txt").getLines().toVector
  val buses =
    lines(1)
      .split(",")
      .toVector
      .zipWithIndex
      .filterNot(_._1 == "x")
      .map { t =>
        val mod = BigInt(t._1.toLong)
        (mod, BigInt(-t._2.toLong) % mod)
      }
  val t = chineseRemainderTheorem(buses)
  println(t._2)
