package com.example.aoc

import scala.collection.mutable.ListBuffer

@main
def Day5(): Unit =  {
  val input = IO.readLines("5.txt").toList
  val lines = input.map(x => {
    val endpoints = x.split("->")
    val point1 = endpoints.head.split(",")
    val point2 = endpoints.last.split(",")
    ((point1.head.trim.toInt, point1.last.trim.toInt), (point2.head.trim.toInt, point2.last.trim.toInt))
  })

  val x1s = lines.map( line => line._1._1).max
  val y1s = lines.map( line => line._1._2).max
  val x2s = lines.map( line => line._2._1).max
  val y2s = lines.map( line => line._2._2).max

  val maxX = x1s.max(x2s)
  val maxY = y1s.max(y2s)


  val grid: List[ListBuffer[Int]] =  List.fill(maxX + 1)(ListBuffer.fill(maxY + 1)(0))

  def range(from: Int, to: Int) = Range.inclusive(from, to, if(from < to) 1 else -1).toList

  lines.foreach(points => {
    if(points._1._1 == points._2._1) { // x1 == x2
      for {
        y <- range(points._2._2, points._1._2) // from miny to maxy
      } do {
        val n = grid(points._1._1)(y)
        grid(points._1._1)(y) = n + 1
      }
    } else if(points._1._2 == points._2._2) { // y1 == y2
      for {
        x <- range(points._2._1, points._1._1) // from minX to maxX
      } do {
        val n = grid(x)(points._1._2)
        grid(x)(points._1._2) = n + 1
      }
    }
    else {
      val slope = (points._2._2 - points._1._2) / (points._2._1 - points._1._1)
      val yIntercept = points._2._2 - (slope * points._2._1)
      if(slope == 1 || slope == -1) {
        for {
          x <- range(points._2._1, points._1._1)
          y <- range(points._1._2, points._2._2)
        } do {
          if(y == ((slope * x) + yIntercept)) // check for processing only the points falling on diagonal
            val n = grid(x)(y)
            grid(x)(y) = n + 1
        }
      }
    }
  })

  val value1 = grid.map(line => line.count(x => x >= 2))
  println(value1.sum)
}
