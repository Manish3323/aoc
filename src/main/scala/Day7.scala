
package com.example.aoc

@main
def Day7(): Unit = {
  def range(from: Int, to: Int) = Range.inclusive(from, to, if(from < to) 1 else -1).toList

  val input = IO.readLines("7.txt").head.split(",").map(_.toInt)

  def calcPart1(elem: Int, elem2: Int) = Math.abs(elem - elem2)


  def calcPart2(elem: Int, elem2: Int) = {
    val n = Math.abs(elem - elem2)
    n * (n + 1) / 2
  }

  def process(calculate: (elem: Int, elem2: Int) => Int): Int = {
    val min = input.min
    val max = input.max
    (min to max).map(elem => {
      input.foldLeft(0)((sum, elem2) => {
       sum + calculate(elem, elem2)
      })
    }).min
  }

  println("Part 1 " +  process(calcPart1))
  println("Part 2 " +  process(calcPart2))
}
