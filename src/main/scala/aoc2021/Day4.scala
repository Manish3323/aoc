
package com.example.aoc

import scala.::
import scala.io.Source

@main
def Day4(): Unit =  {
  val input = IO.readLines("4.txt").toList
  val randomNumbers = input.head.split(",").map(_.toInt)

  def splitAndRemoveEmptyChars(string: String): Array[String] = {
    string.split(" ").filter(x => x != "")
  }

  val grids = input.tail.sliding(6, 6).map(snapOf6lines => snapOf6lines.map(splitAndRemoveEmptyChars)).toList
  val unmarkedGrids = grids.map(_.map(x => x.map(n => (n.toInt, false)).toList)).map(_.filter(x => x.nonEmpty)).map(g => (g, false)).zipWithIndex

  var _markedGrids = unmarkedGrids

  def markElementsIfNumberFound(grid: List[List[(Int, Boolean)]], num: Int) =
    grid.map(row => {
      row.map(n => if (n._1 == num) (n._1, true) else n)
    })

  def markGridColumn(grid: (List[List[(Int, Boolean)]], Boolean), num: Int) =
    val _grid = markElementsIfNumberFound(grid._1.transpose, num)
    (_grid, _grid.exists(row => checkRowCompleted(row)))


  def markGridRow(grid: (List[List[(Int, Boolean)]], Boolean), num: Int) =
    val _grid = markElementsIfNumberFound(grid._1, num)
    (_grid, _grid.exists(r => checkRowCompleted(r)))


  def checkRowCompleted (row: List[(Int, Boolean)]): Boolean = {
    row.forall(n => n._2)
  }

  var allGridsWon: List[(List[List[(Int, Boolean)]], Boolean)] = List.empty
  var allGridsWonList: Set[ Int] = Set.empty

  def addGridToWinnerList(grid: (List[List[(Int, Boolean)]], Boolean), index: Int): Unit = {
    if (grid._2 && !allGridsWonList.contains(index)) {
      allGridsWon = allGridsWon :+ grid
      allGridsWonList = allGridsWonList ++ List(index)
    }
  }

  def sumOfUnmarkedNumbersOf(grid: List[List[(Int, Boolean)]]): Int = {
    grid.foldLeft(0)((sum1, r) => {
      sum1 + r.foldLeft(0)((sum, n) => if (!n._2) sum + n._1 else sum)
    })
  }

  val lastRandomNumber = for {
    randomNumber <- randomNumbers if allGridsWon.size < _markedGrids.length
  } yield {
    _markedGrids = _markedGrids.map((unmarkedGrid, index) => {
      val rowsMarkedInGrid = markGridRow(unmarkedGrid, randomNumber)
      addGridToWinnerList(rowsMarkedInGrid, index)
      val columnsMarkedInGrid = markGridColumn(rowsMarkedInGrid, randomNumber)
      addGridToWinnerList(columnsMarkedInGrid, index)
      (columnsMarkedInGrid, index)
    })
    randomNumber
  }

  val sum = sumOfUnmarkedNumbersOf(allGridsWon.last._1)
  println(" sum : " + sum)
  println(" lastRandom : " + lastRandomNumber.last)
  println(" answer : " + sum * lastRandomNumber.last)

}
