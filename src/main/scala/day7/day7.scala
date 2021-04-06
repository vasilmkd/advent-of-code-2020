package day7

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

type Bag = String

type Graph = Map[Bag, Set[(Int, Bag)]]

def parseSentence(line: String): (Bag, List[(Int, Bag)]) =
  val parts = line.split(" bags contain ")
  val bag = parts(0)
  val rest = parts(1).split(" bags?(, |\\.)")
  val list =
    rest
      .toList
      .filterNot(_ == "no other")
      .map { s =>
        val ps = s.split(" ")
        (ps(0).toInt, ps.drop(1).mkString(" "))
      }
  (bag, list)

def assembleGraph1(graph: Graph, info: (Bag, List[(Int, Bag)])): Graph =
  val (bag, list) = info
  list.foldLeft(graph) { (g, b) =>
    g.updatedWith(b._2)(op => Some(op.getOrElse(Set.empty) + (b._1 -> bag)))
  }

def assembleGraph2(graph: Graph, info: (Bag, List[(Int, Bag)])): Graph =
  val (bag, list) = info
  list.foldLeft(graph) { (g, b) =>
    g.updatedWith(bag)(op => Some(op.getOrElse(Set.empty) + b))
  }

def bfs(graph: Graph, init: Bag): Set[Bag] =
  @tailrec
  def loop(next: Queue[Bag], visited: Set[Bag]): Set[Bag] =
    next.dequeueOption match
      case Some((curr, tail)) =>
        val newVisited = visited + curr
        val reachable = graph.get(curr).getOrElse(Set.empty).map(_._2).filterNot(newVisited)
        loop(tail.enqueueAll(reachable), newVisited)
      case None =>
        visited
  
  loop(Queue(init), Set.empty)

def dfs(graph: Graph, curr: Bag): Long =
  graph.get(curr).getOrElse(Set.empty).toList.map(t => t._1.toLong * dfs(graph, t._2)).sum + 1

@main
def part1(): Unit =
  val graph =
    Source
      .fromResource("day7.txt")
      .getLines()
      .map(parseSentence)
      .foldLeft(Map.empty[Bag, Set[(Int, Bag)]])(assembleGraph1)
  val init = "shiny gold"
  val bags = bfs(graph, init) - init
  println(bags.size)

@main
def part2(): Unit =
  val graph =
    Source
      .fromResource("day7.txt")
      .getLines()
      .map(parseSentence)
      .foldLeft(Map.empty[Bag, Set[(Int, Bag)]])(assembleGraph2)
  val init = "shiny gold"
  val bags = dfs(graph, init) - 1
  println(bags)
