package com.example.aoc
package aoc2021
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

@main
def Day14(): Unit = {
  val input = IO.readLines("14.txt").filter(x => x.nonEmpty)
  val templatePairs = input.head.sliding(2).toList
  val rules = input.tail.map{ case s"$v -> $c" => v -> c }
  val rulesMap = rules.toMap

  val pairs = rules
    .filter { v => templatePairs.contains(v._1) }
    .map(_._1)
    .sortBy(x => templatePairs.indexOf(x))

  pairs.foreach(println)

  val strs = Range(0, 40).foldLeft(pairs)((pairs, i) => {
    val strings = pairs.map(pair => {
      pair
        .sliding(2)
        .foldLeft("")((newString, part) => {
          newString + part.head + rulesMap(part)
        }) + pair.last
    })
    println("step : " + (i + 1))
    strings
  })
  val ans = strs.indices.foldLeft("")((str, i) => {
    val str1 = strs(i)
    if (i == str.length - 1) str + str1 else str + str1.take(str1.length - 1)
  })
  println(ans.length)

  val fMap = ans.foldLeft[Map[Char, Long]](Map.empty)((map, c) => map + (c -> (map.getOrElse(c, 0L) + 1L)))



  //part 1
//  val ans = Range(0, 40).foldLeft(input.head)((input, i) => {
//    input.sliding(2)
//      .map(identity)
//      .foldLeft("")((newString, part) => {
//       newString + part.head + pairs(part)
//      }) + input.last
//  })
//

//  println(fMap.values.max - fMap.values.min)
}
