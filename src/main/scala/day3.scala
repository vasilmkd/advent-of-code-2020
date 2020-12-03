import scala.io.Source

case class FoldState(index: Int, skip: Int, count: Int)

def slopedFn(right: Int, down: Int): (FoldState, String) => FoldState =
  (state, line) =>
    if state.skip > 0 then
      state.copy(skip = state.skip - 1)
    else
      val diff = if line.charAt(state.index) == '#' then 1 else 0
      val cnt = state.count + diff
      val skip = down - 1
      val next = (state.index + right) % line.size
      FoldState(next, skip, cnt)

def countTrees(lines: LazyList[String], slope: (Int, Int)): Long =
  lines.foldLeft(FoldState(0, 0, 0))(slopedFn.tupled(slope)).count.toLong

@main
def day3part1(): Unit =
  val lines = LazyList.from(Source.fromResource("day3.txt").getLines())
  val result = countTrees(lines, (3, 1))
  println(result)

@main
def day3part2(): Unit =
  val lines = LazyList.from(Source.fromResource("day3.txt").getLines())
  val result =
    List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
      .map(countTrees(lines, _))
      .product
  println(result)
