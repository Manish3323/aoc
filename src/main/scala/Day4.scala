
package com.example.aoc

import scala.::
import scala.io.Source

@main
def Day4() =  {
  val input = IO.readLines("4.txt").toList
  val randomNumbers = input.head.split(",").map(_.toInt)
  val grids = input.tail.sliding(6, 6).map(x => x.map(y => y.split(" ").filter(x => x != ""))).toList
  val unmarkedGrids = grids.map(_.map(x => x.map(n => (n.toInt, false)).toList)).map(_.filter(x => x.nonEmpty)).map(g => (g, false)).zipWithIndex

  var _markedGrids = unmarkedGrids

  var allGridsWon: List[(List[List[(Int, Boolean)]], Boolean)] = List.empty
  var allGridsWonList: Set[ Int] = Set.empty
  var lastRandomN = Int.MinValue

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

  def addGridToWinnerList(index: Int, grid: (List[List[(Int, Boolean)]], Boolean)): Unit = {
    if (grid._2 && !allGridsWonList.contains(index)) {
      allGridsWon = allGridsWon :+ grid
      allGridsWonList = allGridsWonList ++ List(index)
    }
  }

  def sumOfUnmarkedNumbersOfBoard(grid: List[List[(Int, Boolean)]]): Int = {
    grid.foldLeft(0)((sum1, r) => {
      sum1 + r.foldLeft(0)((sum, n) => if (!n._2) sum + n._1 else sum)
    })
  }

  for {
    randomNumber <- randomNumbers if allGridsWon.size < _markedGrids.length
  } do {
    _markedGrids = _markedGrids.map((g, index) => {
      val row = markGridRow(g, randomNumber)
      addGridToWinnerList(index, row)
      val grid = markGridColumn(row, randomNumber)
      addGridToWinnerList(index, grid)
      lastRandomN = randomNumber
      (grid, index)
    })
  }
  if(allGridsWon.size == _markedGrids.length) {
    val sum = sumOfUnmarkedNumbersOfBoard(allGridsWon.last._1)
    println(" sum : " + sum )
    println(" lastRandom : " + lastRandomN)
    println(" answer : " + sum * lastRandomN)
  }

}
