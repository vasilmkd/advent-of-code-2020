package day4

import scala.io.Source
import scala.util.Try

type Passport = Map[String, String]

case class State(valid: Int, partial: Passport)

val validators: Map[String, String => Boolean] =
  Map(
    "byr" -> "19[2-9]\\d|200[012]".r.matches,
    "iyr" -> "20(1\\d|20)".r.matches,
    "eyr" -> "20(2\\d|30)".r.matches,
    "hgt" -> "1([5-8]\\d|9[0-3])cm$|(59|6\\d|7[0-6])in$".r.matches,
    "hcl" -> "#[a-f0-9]{6}".r.matches,
    "ecl" -> "amb|blu|brn|gry|grn|hzl|oth".r.matches,
    "pid" -> "\\d{9}".r.matches
  )

def validate1(passport: Passport): Boolean =
  validators.keys.forall(passport.contains)

def validate2(passport: Passport): Boolean =
  validators
    .keys
    .map(key => passport.get(key).filter(validators(key)))
    .forall(_.isDefined)

def foldFn(state: State, line: String, validate: Passport => Boolean): State =
  if line.isEmpty then
    if validate(state.partial) then
      State(state.valid + 1, Map.empty)
    else State(state.valid, Map.empty)
  else
    val parts = line.split(" ")
    val partial =
      parts.map { p =>
        val Array(key, value) = p.split(":")
        (key, value)
      }.toMap
    State(state.valid, state.partial ++ partial)

@main
def part1(): Unit =
  val result = (Source.fromResource("day4.txt").getLines() ++ List(""))
    .foldLeft(State(0, Map.empty))(foldFn(_, _, validate1))
    .valid
  println(result)

@main
def part2(): Unit =
  val result = (Source.fromResource("day4.txt").getLines() ++ List(""))
    .foldLeft(State(0, Map.empty))(foldFn(_, _, validate2))
    .valid
  println(result)
