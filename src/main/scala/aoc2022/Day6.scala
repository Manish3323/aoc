package com.example.aoc
package aoc2022

import IO.{readLines, readLines2}

import scala.language.postfixOps

@main def Day6(): Unit = {
  val line = readLines("2022/6.txt").head

  def problem(n: Int): Unit = {
    val value = line.toCharArray.sliding(n).zipWithIndex.filter(arr => {
      arr._1.distinct.length == arr._1.length
    }).toList.head
    println("Solution: " + (value._2 + n))
  }

  problem(4)
  problem(14)
}