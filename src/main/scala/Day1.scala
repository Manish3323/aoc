package com.example.aoc

import IO.readLines

import scala.language.postfixOps

@main def Day1: Unit = {
  var numbers = readLines("1.txt").map(_.toInt)

  var count = 0
  var previous = numbers(0)
  // 1
  numbers.foreach(x =>
    if(x > previous) count = count + 1
    previous = x
  )
  println(count)

  //**************//

  val count2 = numbers
    .sliding(3)
    .map(_.sum)
    .sliding(2)
    .count { case Seq(a, b) => b > a }

  println(count2)
}




