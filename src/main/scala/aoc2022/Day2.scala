package com.example.aoc
package aoc2022

import IO.readLines

import scala.language.postfixOps

@main def Day1(): Unit = {
  val map: Map[String, Seq[String]] = Map(
    "A" -> Seq("Y", "X", "Z"),
    "B" -> Seq("Z", "Y", "X"),
    "C" -> Seq("X", "Z", "Y"),
  )

  val mapOfIndexToLookup: Map[String, Int] = Map(
    "X" -> 2,
    "Y" -> 1,
    "Z" -> 0,
  )
  val mapOfPoints: Map[String, Int] = Map("X"-> 1, "Y"->2, "Z" ->3)

  val turns: Array[Int] = readLines("2022/2.txt").map(d => {
    val words = d.split(" ")
    val opp = words.head
    val intermediateIndex = words.last
    val index = mapOfIndexToLookup.getOrElse(intermediateIndex, -9)
    val sequenceToLookUp = map.getOrElse(opp, Seq.empty)
    val turn = sequenceToLookUp(index)
    if (index == 0) {
      mapOfPoints(turn) + 6
    }
    else if (index == 1) {
      mapOfPoints(turn) + 3
    }
    else {
      mapOfPoints(turn) + 0
    }
  })
  println(turns.sum)
}