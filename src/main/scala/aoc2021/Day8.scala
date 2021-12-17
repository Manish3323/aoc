package com.example.aoc
package aoc2021

import scala.collection.mutable

@main
def Day8(): Unit = {
  val input = IO.readLines("8.txt")

  val values = input.map(x => {
    val strings = x.split(" /| ")
    val inputs = strings.take(10).map(_.trim)
    val output = strings.takeRight(4).map(_.trim)
    (inputs, output)
  })

  val seqments = IndexedSeq(
    Set('a', 'b', 'c', 'e', 'f', 'g'),
    Set('c', 'f'),
    Set('a', 'c', 'd', 'e', 'g'),
    Set('a', 'c', 'd', 'f', 'g'),
    Set('b', 'c', 'd', 'f'),
    Set('a', 'b', 'd', 'f', 'g'),
    Set('a', 'b', 'd', 'e', 'f', 'g'),
    Set('a', 'c', 'f'),
    Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
    Set('a', 'b', 'c', 'd', 'f', 'g'),
  )
  val chars = ('a' to 'g').permutations.map(x => x -> x).toList

  val x = values.map(t => {
    val value = chars.permutations.foreach(perm => perm.zip(chars).toMap)
//    value.flatMap {
//      assign =>
//        val str = t._1.map(x =>   x.map(assign))
////           pick only that outpput strings whose permutated inputs strings whose segment is within the
//        if (str.forall(v => seqments.contains(v.toSet))) {
//             check newly created sequence is inside seqments
//          Seq(t._2.map(_.map(assign)).map(_.toSet).map(x => seqments.indexOf(x)).toList)
//        } else {
//          Seq.empty
//        }
//    }.toList
  })
//
//  println(x.map(_.map(_.mkString.toInt)).sum)


}
