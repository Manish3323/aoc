
package com.example.aoc

@main
def Day7(): Unit = {
  def range(from: Int, to: Int) = Range.inclusive(from, to, if(from < to) 1 else -1).toList

  val input = IO.readLines("7.txt").head.split(",").map(_.toInt)



  def part1(): Int = {
    input.map(elem => {
      input.foldLeft(0)((sum, elem2) => {
        sum + Math.abs(elem - elem2)
      })
    }).min
  }

  def part2(): Int = {
    val min = input.min
    val max = input.max
    (min to max).map(elem => {
      input.foldLeft(0)((sum, elem2) => {
        sum + range(elem, elem2).indices.map(identity).sum
      })
    }).min
  }

  println("Part 1 " +  part1())
  println("Part 2 " +  part2())
}
