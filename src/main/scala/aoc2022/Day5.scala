package com.example.aoc
package aoc2022


import IO.{readLines, readLines2}

import scala.language.postfixOps

@main def Day5(): Unit = {
  type From = Int
  type To = Int
  type Number = Int
  type Action = (Number, From, To)

  val lines = readLines("2022/5.txt")
  val towersLine = lines.filter(line => line.startsWith(" 1")).head
  val index = lines.indexOf(towersLine)
  val towers = lines.take(index)
  val actionLines = lines.takeRight(lines.length - 1 - (index + 1))
  val actions: Array[Action] = actionLines.map {
    case s"move $n from $from to $to" => (n.toInt, from.toInt, to.toInt)
  }
  val mapOftowers = towers.foldLeft(Map.empty[Int, List[Char]])((map2,line) => {
    val value = line.toCharArray.sliding(3, 4).zipWithIndex.foldLeft(map2)((map, pack) => {
      val curr = map.getOrElse(pack._2 + 1, List.empty[Char])
      val value2 = if pack._1(1) != ' ' then List(pack._1(1)) ++ curr else curr
      map ++ Map(pack._2 + 1 -> value2)
    })
    value
  })
  type Tower = List[Char]
  def problem(actions:  Array[Action], mapOftowers:  Map[Int, Tower], moveItemsInOrder: Boolean): Unit = {
    val tuples = actions.foldLeft(mapOftowers)((towers, action) => {
      val itemsToBeMoved = towers(action._2).takeRight(action._1)
      val orderedItems = if moveItemsInOrder then itemsToBeMoved else itemsToBeMoved.reverse
      val newItems = towers(action._3).appendedAll(orderedItems)
      val removedStack = towers(action._2).dropRight(action._1)
      towers ++ Map(action._2 -> removedStack, action._3 -> newItems)
    })

    val value = tuples.toSeq.sortBy(_._1: _*)
    println(value.map(_._2.last).mkString)
  }

  problem(actions, mapOftowers, false)
  problem(actions, mapOftowers, true)

}