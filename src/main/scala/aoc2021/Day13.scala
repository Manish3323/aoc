package com.example.aoc
package aoc2021
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
@main
def Day13(): Unit = {
  val input = IO.readLines("13.txt").filter(x => x.nonEmpty)
  val (foldInstructionsStrings, dotsString) = input.partition(x => x.contains("fold"))
  val dots = dotsString.map({case s"$i,$j" => (i.toInt,j.toInt)}).toList

  val foldInstructions = foldInstructionsStrings.map {
    case s"fold along x=$x" => ("x", x.toInt)
    case s"fold along y=$y" => ("y", y.toInt)
  }

//  foldInstructions.foreach(println)

  val maxX = dots.map(_._1).max + 1
  val maxY = dots.map(_._2).max + 1
  var filledGrid = ArrayBuffer.fill(maxY, maxX)('.')
  dots.foreach(x => filledGrid(x._2)(x._1) = '#')

//  gridFilled.foreach(x => println(x.mkString("")))
  def foldVertically(x: Int): Unit = {
    for {
      horizontal <- 0 until x
      vertical <- filledGrid.indices
    } yield {
      if(filledGrid(vertical)(horizontal) != '#') {
        filledGrid(vertical)(horizontal) = filledGrid(filledGrid.length - 1 - vertical)(horizontal)
      }else{
        filledGrid(vertical)(horizontal)
      }
    }
    filledGrid = filledGrid.take(x)
    filledGrid.foreach(line => {
      println(line.mkString(","))
    })
  }
  def foldHorizontally(x: Int): Unit = {
    for {
      horizontal <- filledGrid.head.indices
      vertical <- 0 until x
    } do {
      if (filledGrid(vertical)(horizontal) != '#') {
        filledGrid(vertical)(horizontal) = filledGrid(vertical)(filledGrid.head.length - 1 - horizontal)
      } else {
        filledGrid(vertical)(horizontal)
      }
    }
    filledGrid.foreach(line => {
      println(line.mkString(","))
    })
    filledGrid = filledGrid.map(_.take(x))
  }

  def fold(on: String, by: Int): Unit = {
    on match {
      case "x" => foldVertically(by)
      case "y" => foldHorizontally(by)
    }
  }

  def part1(): Unit = {
    val instruction = foldInstructions.head
    fold(instruction._1, instruction._2)
    println("part 1 : " + filledGrid.flatMap(_.filter( _ == '#')).length)
//    filledGrid.foreach(line => {
//      println(line)
//    })
  }

  part1()
  println("*******")
//  grid.foreach(line => {
//    println(line)
// /**/ })

}
