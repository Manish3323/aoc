package com.example.aoc
package aoc2021

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

@main
def Day15(): Unit = {
  type Point = (Int, Int)
  val input = IO.readLines("15.txt")
  val array = input.map(l => l.split("").map(_.toInt))

  def updatedGrid(grid: Array[Array[Int]]): Array[Array[Int]] = {
    val array1 = grid.map(l => {
      Range(1, 5).foldLeft(l)((str, i) => {
        str ++ str.takeRight(grid.head.length).map(x => if(x + 1 > 9) 1 else x + 1)
      })
    })

    Range(1, 5).foldLeft(array1)((newG, i) => {
      newG ++ newG.takeRight(grid.length).map(_.map(x => if (x + 1 > 9) 1 else x + 1))
    })
  }

  def neighbours(p: Point, grid: Array[Array[Int]]) =
    Seq((0, 1), (1, 0), (0, -1), (-1, 0))
      .map(x => (p._1 + x._1, p._2 + x._2))
      .filter(x => grid.indices.contains(x._1) && grid.head.indices.contains(x._2))
      .toArray


  def getLowestPath(p: Point, grid: Array[Array[Int]]) = {
    val risk =  collection.mutable.LinkedHashMap[Point, Int](((0,0), 0))
    val queue = collection.mutable.Queue[Point]((0,0))

    while queue.nonEmpty do {
      val point = queue.dequeue()

      val nn = neighbours(point, grid)
        .foreach(next => {
          if (!risk.contains(next) || risk(point) + grid(next._1)(next._2) < risk(next)) {
            risk(next) = risk(point) + grid(next._1)(next._2)
            queue.enqueue(next)
          }
        })

    }

    risk(grid.length - 1, grid.head.length -1)
  }

  val source = (0, 0)
  val n1 = getLowestPath(source, array)
  val n2 = getLowestPath(source, updatedGrid(array))
  println(n1)
  println(n2)

}
