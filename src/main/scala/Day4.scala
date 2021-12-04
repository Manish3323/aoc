package com.example.aoc

import scala.::

@main
def Day4() =  {
  val input = IO.readLines("4.txt")
  val randomNumbers = input(0).split(",").map(_.toInt)
  val grids = input.takeRight(input.length - 1).toList.sliding(6, 6).map(x => x.map(y => y.split(" ").filter(x => x != ""))).toList
  val unmarkedGrids: List[List[List[(Int, Boolean)]]] = grids.map(_.filter(x => x.nonEmpty).map(x => x.map(n => (n.toInt, false)).toList))
  var _markedGrids = unmarkedGrids.map(_.map(g => (g, false))).zipWithIndex
  var allGridsWon: List[(List[List[(Int, Boolean)]], Boolean)] = List.empty
  var allGridsWonList: Set[ Int] = Set.empty

  
  def markGridColumn(grid: (List[List[(Int, Boolean)]], Boolean), num: Int) = {
    val value = grid._1.transpose.map(row => {
      row.map(n => if (n._1 == num) (n._1, true) else n)
    })
    (value, value.exists(r => checkRowCompleted(r)))
  }
  def markGridRow(grid: (List[List[(Int, Boolean)]], Boolean), num: Int) = {
    val value = grid._1.map(row => {
      row.map(n => if (n._1 == num) (n._1, true) else n)
    })
    (value, value.exists(r => checkRowCompleted(r)))
  }

  def checkRowCompleted (row: List[(Int, Boolean)]): Boolean = {
    row.forall(n => n._2)
  }
  for {
    n <- 0 to randomNumbers.length - 1
  } yield {

  }
  randomNumbers.foreach(randomNumber => {
    _markedGrids = _markedGrids.map((g, i) => {
      val row = markGridRow(g, randomNumber)
      if(row._2 && !allGridsWonList.contains(i)) {
        allGridsWon = allGridsWon :+ row
        allGridsWonList = allGridsWonList ++ List(i)
      }
      val grid = markGridColumn(row, randomNumber)
      if(grid._2 && !allGridsWonList.contains(i)) {
          allGridsWon = allGridsWon :+ grid
          allGridsWonList = allGridsWonList ++ List(i)
      }
      (grid, i)
    })

    if(allGridsWon.size == _markedGrids.length) {
      val value1 = allGridsWon.last._1.foldLeft(0)((sum1, r) => {
        sum1 + r.foldLeft(0)((sum, n) => if (!n._2) sum + n._1 else sum)
      })
      println("found row : index" + allGridsWonList.last +  " :: "  + value1 + ":" + randomNumber + ":" + (value1 * randomNumber))
      allGridsWon.last._1.map(x => println(x.mkString(",")))
      throw Error("found last")
    }
    if(allGridsWon.size == _markedGrids.length) {
      val value1 = allGridsWon.last._1.transpose.foldLeft(0)((sum1, r) => {
        sum1 + r.foldLeft(0)((sum, n) => if (!n._2) sum + n._1 else sum)
      })
      println("found column : index " + allGridsWonList.last +  " :: "  + value1 + ":" + randomNumber + ":" + value1 * randomNumber)
      throw Error("found last")
    }
  })

  def part1(input: Array[String]): Int = 1

  def part2(input: Array[String]): Int = 2
}
