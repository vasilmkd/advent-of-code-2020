package day19

import scala.io.Source
import scala.util.matching.Regex

enum Preprocess:
  case Letter(c: Char)
  case Concats(concats: Vector[String])

trait Parser[+A]:
  def parse(input: String): Option[(A, String)]

  def >>[B](p: => Parser[B]): Parser[B] =
    flatMap(_ => p)

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    Parser(in => parse(in).flatMap(t => f(t._1).parse(t._2)))

  def map[B](f: A => B): Parser[B] =
    flatMap(a => Parser.pure(f(a)))

  def |[B](p: => Parser[B]): Parser[A | B] =
    Parser(in => parse(in).orElse[(A, String) | (B, String)](p.parse(in)))

end Parser

object Parser:
  def apply[A](f: String => Option[(A, String)]): Parser[A] =
    new Parser:
      def parse(input: String): Option[(A, String)] =
        f(input)

  def pure[A](a: A): Parser[A] =
    Parser(in => Some((a, in)))

  def char(c: Char): Parser[String] =
    Parser { in =>
      in
        .headOption
        .collect {
          case h if h == c => (c.toString, in.tail)
        }
    }

  def many[A](p: => Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) | pure(Nil)

  def counted[A](p: => Parser[A]): Parser[Int] =
    many(p).map(_.size)

  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    for
      a <- pa
      b <- pb
    yield f(a, b)

end Parser

def preprocessRule(line: String): (Long, Preprocess) =
  val parts = line.split(": ")
  val index = parts(0).toLong
  val preprocess =
    parts(1) match
      case "\"a\"" => Preprocess.Letter('a')
      case "\"b\"" => Preprocess.Letter('b')
      case rule =>
        val concats = rule.split(" \\| ")
        Preprocess.Concats(concats.toVector)
  (index, preprocess)

def build(preprocessed: Map[Long, Preprocess])(index: Long): Parser[String] =
  preprocessed(index) match
    case Preprocess.Letter(c) => Parser.char(c)
    case Preprocess.Concats(cs) =>
      cs
        .map(_.split(" ").map(_.toLong).map(build(preprocessed)).reduce(_ >> _))
        .reduce(_ | _)

def validate(c1: Int, c2: Int): Boolean =
  c2 >= 1 && c1 > c2

@main
def part1(): Unit =
  val (preprocessed, _, messages) =
    Source
      .fromResource("day19.txt")
      .getLines()
      .foldLeft((Map.empty[Long, Preprocess], false, Vector.empty[String])) {
        (state, line) =>
          if state._2 then (state._1, true, state._3 :+ line)
          else if line.isEmpty then (state._1, true, state._3)
          else (state._1 + preprocessRule(line), false, state._3)
      }
  val parser = build(preprocessed)(0L)
  val res = messages.map(parser.parse).filter(_.filter(_._2.isEmpty).isDefined).count(_.isDefined)
  println(res)

@main
def part2(): Unit =
  val (preprocessed, _, messages) =
    Source
      .fromResource("day19.txt")
      .getLines()
      .foldLeft((Map.empty[Long, Preprocess], false, Vector.empty[String])) {
        (state, line) =>
          if state._2 then (state._1, true, state._3 :+ line)
          else if line.isEmpty then (state._1, true, state._3)
          else (state._1 + preprocessRule(line), false, state._3)
      }

  val parser =
    Parser.map2(Parser.counted(build(preprocessed)(42L)), Parser.counted(build(preprocessed)(31L)))(validate) 

  val res = messages.map(parser.parse).filter(_.filter(t => t._1 && t._2.isEmpty).isDefined).count(_.isDefined)
  println(res)
