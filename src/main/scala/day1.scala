import scala.io.Source

def fn(prev: Set[Int], n: Int, f: Int => Int): Either[Set[Int], Int] =
  val other = f(n)
  if prev(other) then Right(other * n) else Left(prev + n)

def search(numbers: IterableOnce[Int], f: Int => Int): Option[Int] =
  LazyList.from(numbers).scanLeft[Either[Set[Int], Int]](Left(Set.empty)):
    case (Left(set), n) => fn(set, n, f)
    case (Right(res), _) => Right(res)
  .find(_.isRight).flatMap(_.toOption)

@main
def day1part1(): Unit =
  val numbers = Source.fromResource("day1.txt").getLines().map(_.toInt)
  val result = search(numbers, 2020 - _)
  println(result)

@main
def day1part2(): Unit =
  val numbers = Source.fromResource("day1.txt").getLines().map(_.toInt).toVector
  val result =
    LazyList
      .from(numbers)
      .map(n => (n, search(numbers, 2020 - n - _)))
      .find(_._2.isDefined)
      .collect:
        case (n, Some(other)) => n * other
  println(result)
