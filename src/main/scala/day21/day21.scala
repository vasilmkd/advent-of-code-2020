package day21

import scala.io.Source
import scala.util.matching.Regex

case class Rule(ingredients: Vector[String], allergens: Vector[String])

val lineRegex: Regex = """^([a-z ]*) \(contains ([a-z ,]*)\)""".r

def parseRole(line: String): Rule =
  line match
    case lineRegex(ingredients, allergens) =>
      Rule(ingredients.split(" ").toVector, allergens.split(", ").toVector)
    case _ =>
      sys.error("unreachable")

def iteration(mapping: Map[String, Vector[String]]): Map[String, Vector[String]] =
  val candidates =
    mapping
      .collect {
        case (a, Vector(i)) => (a, i)
      }
      .toVector
  candidates
    .foldLeft(mapping) {
      case (acc, (a, i)) =>
        acc.map { (all, ings) =>
          if all == a then (all, ings)
          else (all, ings.filterNot(_ == i))
        }
    }

@main
def part1(): Unit =
  val rules =
    Source
      .fromResource("day21.txt")
      .getLines()
      .map(parseRole)
      .toVector

  var mapping =
    rules
      .foldLeft(Map.empty[String, Vector[String]]) { (acc, r) =>
        r.allergens.foldLeft(acc) { (acc, a) =>
          acc.updatedWith(a) {
            case Some(is) => Some(r.ingredients.intersect(is))
            case None => Some(r.ingredients)
          }
        }
      }

  LazyList
    .iterate(iteration(mapping))(iteration)
    .takeWhile(_ != mapping)
    .tapEach(mapping = _)
    .last

  val containAllergens = mapping.values.foldLeft(Set.empty[String])(_ union _.toSet)
  val result =
    rules
      .map(_.ingredients.filterNot(containAllergens))
      .map(_.size)
      .sum

  println(result)

@main
def part2(): Unit =
  val rules =
    Source
      .fromResource("day21.txt")
      .getLines()
      .map(parseRole)
      .toVector

  var mapping =
    rules
      .foldLeft(Map.empty[String, Vector[String]]) { (acc, r) =>
        r.allergens.foldLeft(acc) { (acc, a) =>
          acc.updatedWith(a) {
            case Some(is) => Some(r.ingredients.intersect(is))
            case None => Some(r.ingredients)
          }
        }
      }

  LazyList
    .iterate(iteration(mapping))(iteration)
    .takeWhile(_ != mapping)
    .tapEach(mapping = _)
    .last

  val blah =
    mapping.collect {
      case (a, Vector(i)) => (a, i)
    }
  val result = blah.toVector.sortBy(_._1).map(_._2).mkString(",")

  println(result)


