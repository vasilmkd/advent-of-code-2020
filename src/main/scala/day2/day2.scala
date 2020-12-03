package day2

import scala.io.Source

case class Config(letter: Char, min: Int, max: Int)

def parseConfig(cfg: String): Config =
  val parts = cfg.split(" ")
  val letter = parts(1)(0)
  val minMax = parts(0).split("-")
  val min = minMax(0).toInt
  val max = minMax(1).toInt
  Config(letter, min, max)

def parse(line: String): (Config, String) =
  val parts = line.split(": ")
  (parseConfig(parts(0)), parts(1))

def validate1(config: Config, password: String): Boolean =
  val cnt = password.count(_ == config.letter)
  cnt >= config.min && cnt <= config.max

def validate2(config: Config, password: String): Boolean =
  val fst = password(config.min - 1) == config.letter
  val snd = password(config.max - 1) == config.letter
  fst ^ snd

@main
def part1(): Unit =
  val result = Source.fromResource("day2.txt").getLines().count(parse andThen validate1)
  println(result)

@main
def part2(): Unit =
  val result = Source.fromResource("day2.txt").getLines().count(parse andThen validate2)
  println(result)
