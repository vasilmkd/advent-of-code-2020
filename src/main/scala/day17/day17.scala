package day17

import scala.io.Source
import scala.util.Try

type Universe3 = Map[Cube, Boolean]

type Universe4 = Map[Hypercube, Boolean]

type Cube = (Long, Long, Long)

type Hypercube = (Long, Long, Long, Long)

def sumCube(t1: Cube, t2: Cube): Cube =
  (t1, t2) match
    case ((a, b, c), (d, e, f)) => (a + d, b + e, c + f)

def sumHypercube(t1: Hypercube, t2: Hypercube): Hypercube =
  (t1, t2) match
    case ((a, b, c, d), (e, f, g, h)) => (a + e, b + f, c + g, d + h)

def neighboring(x: Long, y: Long, z: Long): Vector[Cube] =
  val range = (-1L to 1L).toVector
  val diffs =
    for
      dx <- range
      dy <- range
      dz <- range
    yield (dx, dy, dz)
  diffs.filterNot(_ == (0L, 0L, 0L)).map(sumCube(_, (x, y, z)))

def neighboring(x: Long, y: Long, z: Long, w: Long): Vector[Hypercube] =
  val range = (-1L to 1L).toVector
  val diffs =
    for
      dx <- range
      dy <- range
      dz <- range
      dw <- range
    yield (dx, dy, dz, dw)
  diffs.filterNot(_ == (0L, 0L, 0L, 0L)).map(sumHypercube(_, (x, y, z, w)))

def active(universe: Universe3)(x: Long, y: Long, z: Long): Boolean =
  universe.get((x, y, z)).getOrElse(false)

def active(universe: Universe4)(x: Long, y: Long, z: Long, w: Long): Boolean =
  universe.get((x, y, z, w)).getOrElse(false)

def rule1(universe: Universe3)(x: Long, y: Long, z: Long): Boolean =
  lazy val checkCount: Long =
    neighboring(x, y, z).count(active(universe).tupled)
  active(universe)(x, y, z) && (checkCount == 2 || checkCount == 3)

def rule1(universe: Universe4)(x: Long, y: Long, z: Long, w: Long): Boolean =
  lazy val checkCount: Long =
    neighboring(x, y, z, w).count(active(universe).tupled)
  active(universe)(x, y, z, w) && (checkCount == 2 || checkCount == 3)

def rule2(universe: Universe3)(x: Long, y: Long, z: Long): Boolean =
  lazy val checkCount: Long =
    neighboring(x, y, z).count(active(universe).tupled)
  !active(universe)(x, y, z) && checkCount == 3

def rule2(universe: Universe4)(x: Long, y: Long, z: Long, w: Long): Boolean =
  lazy val checkCount: Long =
    neighboring(x, y, z, w).count(active(universe).tupled)
  !active(universe)(x, y, z, w) && checkCount == 3

def parse3(map: Array[Array[Char]]): Universe3 =
  map
    .zipWithIndex
    .map:
      (r, i) =>
        r.zipWithIndex
          .foldLeft(Map.empty[Cube, Boolean]):
            case (acc, (c, j)) =>
              if c == '#' then acc + ((i.toLong, j.toLong, 0L) -> true) else acc
    .foldLeft(Map.empty[Cube, Boolean])(_ ++ _)

def parse4(map: Array[Array[Char]]): Universe4 =
  map
    .zipWithIndex
    .map:
      (r, i) =>
        r.zipWithIndex
          .foldLeft(Map.empty[Hypercube, Boolean]):
            case (acc, (c, j)) =>
              if c == '#' then acc + ((i.toLong, j.toLong, 0L, 0L) -> true) else acc
    .foldLeft(Map.empty[Hypercube, Boolean])(_ ++ _)

def iteration3(universe: Universe3): Universe3 =
  val blah = universe.map:
    case ((x, y, z), _) =>
      val activated = neighboring(x, y, z).filter(rule2(universe).tupled)
      if rule1(universe)(x, y, z) then activated :+ (x, y, z) else activated
  blah.foldLeft(Map.empty[Cube, Boolean])(_ ++ _.map(t => (t -> true)))

def iteration4(universe: Universe4): Universe4 =
  val blah = universe.map:
    case ((x, y, z, w), _) =>
      val activated = neighboring(x, y, z, w).filter(rule2(universe).tupled)
      if rule1(universe)(x, y, z, w) then activated :+ (x, y, z, w) else activated
  blah.foldLeft(Map.empty[Hypercube, Boolean])(_ ++ _.map(t => (t -> true)))
  
@main
def part1(): Unit =
  val map = Source.fromResource("day17.txt").getLines().map(_.toCharArray).toArray
  val start = parse3(map)
  val size =
    LazyList
      .iterate(start)(iteration3)
      .take(7)
      .last
      .size
  println(size)

@main
def part2(): Unit =
  val map = Source.fromResource("day17.txt").getLines().map(_.toCharArray).toArray
  val start = parse4(map)
  val size =
    LazyList
      .iterate(start)(iteration4)
      .take(7)
      .last
      .size
  println(size)
