package day16

import scala.collection.immutable.NumericRange
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex

trait Monoid[A]:
  def identity: A
  extension (x: A)
    def combine(y: A): A
    final def |+| (y: A): A = combine(y)

object Monoid:
  given [A: Monoid] as Monoid[Option[A]]:
    val identity: Option[A] = Some(summon[Monoid[A]].identity)
    extension (x: Option[A])
      def combine(y: Option[A]): Option[A] =
        for
          a1 <- x
          a2 <- y
        yield a1 |+| a2

  given Monoid[Long]:
    val identity: Long = 0L
    extension (x: Long)
      def combine(y: Long): Long =
        x + y

trait LeftFoldable[F[_]]:
  extension[A, B] (fa: F[A])
    def leftFold(b: B)(f: (B, A) => B): B

  extension[A: Monoid] (fa: F[A])
    def leftFoldMonoid: A =
      fa.leftFold(summon[Monoid[A]].identity)(_ |+| _)

object LeftFoldable:
  given LeftFoldable[Vector]:
    extension[A, B] (fa: Vector[A])
      def leftFold(b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

enum ParseState:
  case Rules(rules: Vector[Rule])
  case MyTicketHeader(rules: Vector[Rule])
  case MyTicket(rules: Vector[Rule], myTicket: Ticket)
  case NearbyTickets(rules: Vector[Rule], myTicket: Ticket, nearbyTickets: Vector[Ticket])

case class Rule(name: String, range1: NumericRange.Inclusive[Long], range2: NumericRange.Inclusive[Long]):
  def contains(n: Long): Boolean =
    range1.contains(n) || range2.contains(n)

case class Ticket(positions: Vector[Long]) extends AnyVal

val ruleRegex: Regex = """([a-z ]*): (\d*)-(\d*) or (\d*)-(\d*)""".r

def parseRule(line: String): Option[Rule] =
  line match
    case ruleRegex(name, r1s, r1e, r2s, r2e) =>
      Some(Rule(name, r1s.toLong to r1e.toLong, r2s.toLong to r2e.toLong))
    case _ => None

def parseTicket(line: String): Option[Ticket] =
  Try(Ticket(line.split(",").map(_.toLong).toVector)).toOption

def parseHeader(header: String)(line: String): Option[header.type] =
  Some[header.type](header).filter(_ == line)

def parseMyTicketHeader(line: String): Option["your ticket:"] =
  parseHeader("your ticket:")(line)

def parseNearbyTicketsHeader(line: String): Option["nearby tickets:"] =
  parseHeader("nearby tickets:")(line)

def parseLine(line: String): Rule | Ticket | "your ticket:" | "nearby tickets:" | "" =
  parseRule(line)
    .orElse[Rule | Ticket](parseTicket(line))
    .orElse[Rule | Ticket | "your ticket:"](parseMyTicketHeader(line))
    .orElse[Rule | Ticket | "your ticket:" | "nearby tickets:"](parseNearbyTicketsHeader(line))
    .getOrElse("")

def parseFoldFn(state: ParseState, line: String): ParseState =
  (state, parseLine(line)) match
    case (ParseState.Rules(rules), r as Rule(_, _, _)) => ParseState.Rules(rules :+ r)
    case (ParseState.Rules(rules), "your ticket:") => ParseState.MyTicketHeader(rules)
    case (ParseState.MyTicketHeader(rules), t as Ticket(_)) => ParseState.MyTicket(rules, t)
    case (ParseState.MyTicket(rules, mt), "nearby tickets:") => ParseState.NearbyTickets(rules, mt, Vector.empty)
    case (ParseState.NearbyTickets(rules, mt, nts), t as Ticket(_)) => ParseState.NearbyTickets(rules, mt, nts :+ t)
    case _ => state

def validateTicket(rules: Vector[Rule])(ticket: Ticket): Option[Long] =
  import LeftFoldable.given
  ticket
    .positions
    .map(t => Some(t).filterNot(t => rules.exists(_.contains(t))))
    .filter(_.isDefined)
    .leftFoldMonoid

def validTicket(rules: Vector[Rule])(ticket: Ticket): Boolean =
  ticket.positions.forall(p => rules.exists(_.contains(p)))

def transposeTickets(tickets: Vector[Ticket]): Vector[Vector[Long]] =
  for
    i <- (0 until tickets(0).positions.size).toVector
  yield tickets.map(_.positions(i))

def validatePositions(valid: Vector[Rule])(positions: Vector[Long]): Vector[String] =
  valid.filter(r => positions.forall(r.contains)).map(_.name)

def dfs(valid: Vector[Rule], positions: Vector[Vector[Long]]): Vector[String] =
  def loop(valid: Vector[Rule], positions: Vector[Vector[Long]]): Option[Vector[String]] =
    if valid.isEmpty && positions.isEmpty then Some(Vector.empty)
    else if valid.isEmpty then None
    else if positions.isEmpty then None
    else
      val possible = validatePositions(valid)(positions(0))
      possible
        .map(p => (p, loop(valid.filterNot(_.name == p), positions.tail)))
        .find(_._2.isDefined)
        .flatMap:
          (p, op) => op.map(p +: _)
  loop(valid, positions).get

@main
def part1(): Unit =
  val parse = Source.fromResource("day16.txt").getLines().foldLeft(ParseState.Rules(Vector.empty))(parseFoldFn)
  parse match
    case ParseState.NearbyTickets(rules, mt, nts) =>
      import LeftFoldable.given
      val result = nts.map(validateTicket(rules)).filter(_.filterNot(_ == 0L).isDefined).leftFoldMonoid
      println(result)
    case _ => sys.error("unreachable")

@main
def part2(): Unit =
  val parse = Source.fromResource("day16.txt").getLines().foldLeft(ParseState.Rules(Vector.empty))(parseFoldFn)
  parse match
    case ParseState.NearbyTickets(rules, mt, nts) =>
      val valid = nts.filter(validTicket(rules))
      val positions = transposeTickets(valid).zipWithIndex.sortBy(t => validatePositions(rules)(t._1).size)
      val search = dfs(rules, positions.map(_._1))
      val result = search.zip(positions.map(_._2)).filter(_._1.startsWith("departure")).map(t => mt.positions(t._2)).product
      println(result)
    case _ => sys.error("unreachable")
