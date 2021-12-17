package com.example.aoc
package aoc2021

import scala.:+
import scala.annotation.tailrec
import scala.collection.mutable

@main
def Day9(): Unit = {
  val input = IO.readLines("9.txt")
  val positions = input.map(_.map(_.toString.toInt))

  // part 1
  val cornerPoints = List((0, 0), (0, positions.head.length - 1),  (positions.length - 1, 0), (positions.length-1, positions.head.length -1))
  val edgeXs = List(0, positions.length -1)
  val edgeYs = List(0, positions.head.length -1)

  def getNeighbours(point: (Int, Int)): Set[(Int, Int)] =
    Seq((-1, 0), (1, 0), (0, -1), (0, 1))
      .map((i1, j1) => (i1 + point._1, j1 + point._2))
      .filter(x =>
        positions.indices.contains(x._1)
        && positions.head.indices.contains(x._2)
        && positions(point._1)(point._2) < positions(x._1)(x._2)
      ).toSet

  val s: Seq[IndexedSeq[((Int, Int), Boolean)]] = positions.indices.map(x => {
    val line = positions(x)
    for {
      y <- line.indices
    } yield {
      val point = positions(x)(y)

      val value: ((Int, Int), Boolean) =
        if (!cornerPoints.contains((x, y)) && !edgeXs.contains(x) && !edgeYs.contains(y)) {
        (x, y) -> (positions(x)(y) < positions(x)(y - 1) && point < positions(x + 1)(y) && point < positions(x)(y + 1) && point < positions(x - 1)(y))
      } else if (cornerPoints.contains((x, y))) {
        if ((x, y) == cornerPoints.head) {
          (x, y) -> (point < positions(x)(y + 1) && point < positions(x + 1)(y))
        } else if ((x, y) == cornerPoints.last) {
          (x, y) -> (point < positions(x)(y - 1) && point < positions(x - 1)(y))
        } else if ((x, y) == cornerPoints(1)) {
          (x, y) -> (point < positions(x + 1)(y) && point < positions(x)(y - 1))
        } else {
          (x, y) -> (point < positions(x - 1)(y) && point < positions(x)(y + 1))
        }
      } else if (edgeXs.contains(x) || edgeYs.contains(y)) {
        if (edgeXs.contains(x)) {
          if (x == 0) (x, y) -> (point < positions(x + 1)(y) && point < positions(x)(y - 1) && point < positions(x)(y + 1))
          else (x, y) -> (point < positions(x - 1)(y) && point < positions(x)(y + 1) && point < positions(x)(y - 1))
        } else if (edgeYs.contains(y)) {
          if (y == 0) (x, y) -> (point < positions(x - 1)(y) && point < positions(x + 1)(y) && point < positions(x)(y + 1))
          else (x, y) -> (point < positions(x - 1)(y) && point < positions(x + 1)(y) && point < positions(x)(y - 1))
        }else{
          (x, y) -> (point < positions(x)(y - 1) && point < positions(x + 1)(y) && point < positions(x)(y + 1) && point < positions(x - 1)(y))
        }
      }else {
        (x, y) -> (point < positions(x)(y - 1) && point < positions(x + 1)(y) && point < positions(x)(y + 1) && point < positions(x - 1)(y))
      }
      value
    }
  })

  val lowestPoints = s.flatMap(x => x.filter(_._2))
  println("part 1: " + lowestPoints.foldLeft(0)((sum, x) => {
    sum + positions(x._1._1)(x._1._2) + 1
  }))
  //part 2
//
//  def getNeighbours(point: (Int, Int)): Set[(Int, Int)] =
//    Seq((-1, 0), (1, 0), (0, -1), (0, 1))
//      .map((i1, j1) => (i1 + point._1, j1 + point._2))
//      .filter(x =>
//        positions.indices.contains(x._1)
//        && positions.head.indices.contains(x._2)
//        && positions(point._1)(point._2) < positions(x._1)(x._2)
//        && positions(x._1)(x._2) != 9
//      ).toSet

  @tailrec
  def fillBasin(lowest: Set[(Int, Int)], neighbours: Set[(Int,Int)]): Set[(Int, Int)] = {
    var basin = lowest
    var newNeighboursFound = Set.empty[(Int,Int)]
    if(neighbours.isEmpty) basin
    else {
      neighbours.foreach(n => {
        getNeighbours(n).foreach(nn => {
          if (!basin.contains(nn)) {
            newNeighboursFound = newNeighboursFound ++ Set(nn)
            basin = basin ++ Set(nn)
          }
        })
      })
      fillBasin(basin, newNeighboursFound)
    }
  }


  val product = lowestPoints.map(_._1).map(low => {
    fillBasin(Set(low), Set(low)).size
  }).sorted.takeRight(3).product
  println("Part 2: " + product)
}
