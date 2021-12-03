package com.example.aoc

import scala.annotation.tailrec

def parseInt(string: String): Int = {
  Integer.parseInt(string, 2)
}

@main def Day3(): Unit = {
  val input: Array[String] = IO.readLines("3.txt")
  val rowLength = input(0).toString.length
  val columns = Array.ofDim[String](rowLength , input.length)

  def part2(reports: Array[String]): Unit = {
    @tailrec
    def filterReports(reports: Array[String], index: Int, charToBeGreater: Char, charToBeLesser: Char): Int = {
      val zeroCount = reports.count(x => x.charAt(index) == '0')
      val oneCounts = reports.count(x => x.charAt(index) == '1')
      val filterByChar = if (zeroCount > oneCounts) charToBeGreater else charToBeLesser

      val filteredReports = reports.filter(_.charAt(index) == filterByChar)

      if (filteredReports.length == 1) parseInt(filteredReports.head)
      else filterReports(filteredReports, index + 1, charToBeGreater, charToBeLesser)
    }
    val oxygenCount = filterReports(input, 0, '1', '0')
    val carbonCount = filterReports(input, 0, '0', '1')
    print("Part 2 answer: " + oxygenCount * carbonCount)
  }

  def part1(input: List[String]): Unit = {
    val transposedReports = input.transpose
    val d1 = transposedReports.map(x => (0, x.count { y => y == '0' }))
    val d2 = transposedReports.map(x => (1, x.count { y => y == '1' }))

    def calculate(listOf0s: Seq[(Int, Int)], listOf1s: Seq[(Int, Int)], mostCommonBit: Int, lessCommonBit: Int) = {
      val sequence = for {
        i <- listOf0s.indices
      } yield {
        val onesCount = listOf1s(i)._2
        val zeroCount = listOf0s(i)._2
        if (zeroCount > onesCount) mostCommonBit else lessCommonBit
      }
      parseInt(sequence.mkString)
    }
    val gammaRate = calculate(d1, d2, 0, 1)
    val epsilonRate = calculate(d1, d2, 1, 0)
    val powerConsumption = gammaRate * epsilonRate
    print("Part 1 : answer " + powerConsumption)
  }

  //
  part1(input.toList)
  part2(input)
}
