package com.example.aoc
package aoc2021

import scala.annotation.tailrec

@main
def Day11(): Unit = {
  val input: List[List[Int]] = IO.readLines("11.txt").map(x => x.map(_.toString.toInt).toList).toList

  def getNeighbours(point: (Int, Int)): List[(Int, Int)] =
    List((-1, 0), (1, 0), (0, -1), (0, 1),(1,1), (-1, 1), (1,-1), (-1, -1))
      .map((i1, j1) => (i1 + point._1, j1 + point._2))
      .filter(x =>
        input.indices.contains(x._1)
          && input.head.indices.contains(x._2)
      )

  def updateNeighbours(octoposes: List[List[Int]], point: (Int, Int), visitedZeros: Set[(Int, Int)]): (List[List[Int]], Set[(Int, Int)]) = {
    var gg = octoposes
    var listOfVisitedZeros = visitedZeros
    val value = gg(point._1)(point._2)
    val neighbours = getNeighbours(point)
    if(value == 0){
      if(neighbours.forall(x => gg(x._1)(x._2) == 0)){
        gg = gg.updated(point._1, gg(point._1).updated(point._2, 0))
      }
      neighbours.foreach(n => {
        if(!listOfVisitedZeros.contains(n)){
          listOfVisitedZeros = listOfVisitedZeros + n
          val tuple = updateNeighbours(gg, n, listOfVisitedZeros)
          gg = tuple._1
          listOfVisitedZeros = tuple._2
        }
      })
      (gg,listOfVisitedZeros)
    }else{
      val zeros = neighbours.count(x => gg(x._1)(x._2) == 0)
      val i = if (value + zeros > 9) 0  else value + 1
      if(i == 0){
        listOfVisitedZeros = listOfVisitedZeros + point
      }
      gg = gg.updated(point._1, gg(point._1).updated(point._2, i))
      (gg,listOfVisitedZeros)
    }
  }
  @tailrec
  def step(octopuses: List[List[Int]], count: Int, visitedZeros: Set[(Int,Int)]): List[List[Int]] = {
    println(s"step ${count}")
    var listOfVisitedZeros = visitedZeros
    var gg = octopuses.map(x=> x.map(y => if(y + 1 > 9) 0 else y + 1))
    for {
      i <- octopuses.indices
      j <- octopuses.head.indices
    } do {
//      gg.foreach(line => println(line.mkString(",")))
      val increasedBy1 = gg(i)(j)
      if(increasedBy1 == 0){
        val tuple = updateNeighbours(gg, (i,j), listOfVisitedZeros)
        gg = tuple._1
        listOfVisitedZeros = tuple._2
      }
    }
    println(s"printing gird after step : ${count}")
    println("____")
    gg.foreach(line => println(line.mkString(",")))

    if (count <= 0) gg
    else step(gg, count - 1, listOfVisitedZeros)
  }

  step(input, 1,Set.empty[(Int, Int)])

}
