package com.example.aoc.aoc2021

import java.util
import scala.collection.mutable
import scala.util.chaining.*

@main
def Day12() : Unit = {
  val input = IO.readLines("12.txt")

  val graph = input.flatMap {
    case s"$l-$r" => Seq(l -> r, r -> l)
  }.groupMap((l, _) => l)((_, r) => r)

  graph.foreach(g => println(g._1 + "-> " + g._2.mkString(",")))
//  val startNodes = input.filter(x =>x.contains("start")).map(x=> x.filterNot(_.contains("start")).head)
//  val endNodes = input.filter(x =>x.contains("end")).map(x=> x.filterNot(_.contains("end")).head)
//  val validPaths = input.filter(x => !(x.contains("end") || x.contains("start")))
//  val smallCaves = input.flatMap(x => x.filter(y => y.toLowerCase == y && y != "start" && y != "end"))
//  val bigCaves = input.flatMap(x => x.filter(y => y.toUpperCase() == y && y != "start" && y != "end"))
//  println(startNodes.mkString(","))
//  println(endNodes.mkString(","))
//  (startNodes ++ endNodes).permutations.foreach(x => println(x.mkString(",")))
//
//  val paths: mutable.Set[List[String]] = mutable.Set.empty[List[String]]

//  (startNodes zip endNodes).map(s => {
//    val l = util.LinkedList[String]()
//    l.addFirst(s._1)
//
//    l.addLast(s._2)
//    l
//  })

  def isLarge(s: String): Boolean = s.forall(_.isUpper)
  val (start, end) = ("start", "end")

  def count(f: Seq[String] => Boolean, path: Seq[String] = Seq(start)): Int =
    val s = path.head
    if s == end then
      1
    else
      graph(s).filter(t => t != start && (isLarge(t) || !path.contains(t) || f(path))).map(t => count(f, t +: path)).sum

   println(count(_ => false))

   println(count(_.filterNot(isLarge).pipe(d => d.distinct.size == d.size)))

  //generate all possible paths
  //keep a path with atleast one small cave

}
