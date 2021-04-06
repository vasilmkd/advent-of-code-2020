package day18

import scala.io.Source
import scala.util.Try

enum Expression:
  case Value(n: Long)
  case Sum(left: Expression, right: Expression)
  case Product(left: Expression, right: Expression)
  case Empty

trait Parser[+A]:
  def parse(input: String): Option[(A, String)]

  def ? : Parser[Option[A]] =
    map(Some(_)).handleErrorWith(Parser.pure(None))

  def |[B](p: => Parser[B]): Parser[A | B] =
    handleErrorWith[A | B](p)

  def *>[B](p: Parser[B]): Parser[B] =
    flatMap(_ => p)

  def >>[B](p: => Parser[B]): Parser[B] =
    flatMap(_ => p)

  def as[B](b: B): Parser[B] =
    map(_ => b)

  def filter(p: A => Boolean): Parser[A] =
    flatMap(a => if p(a) then Parser.pure(a) else Parser.raiseError)

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    Parser(in => parse(in).flatMap((a, tl) => f(a).parse(tl)))

  def handleErrorWith[B >: A](p: => Parser[B]): Parser[B] =
    Parser { in =>
      parse(in) match
        case None => p.parse(in)
        case some => some
    }

  def map[B](f: A => B): Parser[B] =
    flatMap(a => Parser.pure(f(a)))
end Parser

object Parser:
  def apply[A](f: String => Option[(A, String)]): Parser[A] =
    new Parser[A]:
      def parse(input: String): Option[(A, String)] =
        f(input)

  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    for
      a <- pa
      b <- pb
    yield f(a, b)

  def pure[A](a: A): Parser[A] =
    Parser(in => Some((a, in)))

  def raiseError[A]: Parser[A] =
    Parser(_ => None)

  def next: Parser[Char] =
    Parser(in => in.headOption.map((_, in.tail)))

  def char(c: Char): Parser[c.type] =
    next.filter(_ == c).map(_.asInstanceOf[c.type])

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) | Parser.pure(Nil)
end Parser

// Common parsers

def value: Parser[Expression.Value] =
  Parser.next.flatMap { c =>
    Parser(in => Try(c.toString.toLong).toOption.map(l => (Expression.Value(l), in)))
  }

def plus: Parser[" + "] =
  (Parser.char(' ') >> Parser.char('+') >> Parser.char(' ')).as(" + ")

def times: Parser[" * "] =
  (Parser.char(' ') >> Parser.char('*') >> Parser.char(' ')).as(" * ")

// Part 1 left associative parsers

def sum1: Parser[Expression.Sum] =
  Parser.map2(value | parens1, plus)((e, _) => Expression.Sum(e, Expression.Empty))

def product1: Parser[Expression.Product] =
  Parser.map2(value | parens1, times)((e, _) => Expression.Product(e, Expression.Empty))

def parens1: Parser[Expression] =
  for
    _ <- Parser.char('(')
    e <- expression1
    _ <- Parser.char(')')
  yield e

def expression1: Parser[Expression] =
  Parser.map2(Parser.many(sum1 | product1), value | parens1) { (sorps, rhs) =>
      val root =
        sorps
          .reduce {
            (acc, sorp) =>
              acc match
                case Expression.Sum(lhs, _) =>
                  sorp match
                    case Expression.Sum(l, e) => Expression.Sum(Expression.Sum(lhs, l), e)
                    case Expression.Product(l, e) => Expression.Product(Expression.Sum(lhs, l), e)
                case Expression.Product(lhs, _) =>
                  sorp match
                    case Expression.Sum(l, e) => Expression.Sum(Expression.Product(lhs, l), e)
                    case Expression.Product(l, e) => Expression.Product(Expression.Product(lhs, l), e)
          }
      root match
        case Expression.Sum(lhs, _) => Expression.Sum(lhs, rhs)
        case Expression.Product(lhs, _) => Expression.Product(lhs, rhs)
  }

// Part 2 recursive left associative parsers

def sum2: Parser[Expression] =
  val expr = value | parens2
  Parser.map2(expr, Parser.many((plus >> expr).map[Expression.Sum](e => Expression.Sum(Expression.Empty, e)))) {
    case (l, rhss) => rhss.foldLeft(l)((acc, r) => r.copy(left = acc))
  }

def product2: Parser[Expression] =
  val expr = sum2 | value | parens2
  Parser.map2(expr, Parser.many((times >> expr).map[Expression.Product](e => Expression.Product(Expression.Empty, e)))) {
    case (l, rhss) => rhss.foldLeft(l)((acc, r) => r.copy(left = acc))
  }

def parens2: Parser[Expression] =
  for
    _ <- Parser.char('(')
    e <- expression2
    _ <- Parser.char(')')
  yield e

def expression2: Parser[Expression] =
  product2

// Evaluation methods

def parse[A](p: Parser[A], input: String): Option[A] =
  p.parse(input).filter(_._2.isEmpty).map(_._1)

def eval(expr: Expression): Long =
  expr match
    case Expression.Value(v) => v
    case Expression.Sum(l, r) => eval(l) + eval(r)
    case Expression.Product(l, r) => eval(l) * eval(r)
    case Expression.Empty => sys.error("blah")

@main
def part1(): Unit =
  val res =
    Source
      .fromResource("day18.txt")
      .getLines()
      .map(parse(expression1, _))
      .collect {
        case Some(e) => e
      }
      .map(eval)
      .sum
  println(res)

@main
def part2(): Unit =
  val res =
    Source
      .fromResource("day18.txt")
      .getLines()
      .map(parse(expression2, _))
      .collect {
        case Some(e) => e
      }
      .map(eval)
      .sum
  println(res)
