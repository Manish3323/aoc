package com.example.aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

@main
def Day6(): Unit = {
  val input = IO.readLines("6.txt")
  val initialState = input.head.split(",").map(_.toInt)
  val r = Range(0, 9).toList.map(i=>(i,0L)).toMap
  val freqm = mutable.Map.from(r)
  r.map(r => initialState.contains(r._1))
  initialState.foreach(i => {
    freqm(i) = freqm(i) + 1L
  })

//
//  for {
//    day <- 0 to 17
//  } yield {
//    freqm.map(k => {
//      k._1 match {
//        case 0 => freqm(1)
//        case 1 => freqm(2)
//        case 2 => freqm(3)
//        case 3 => freqm(4)
//        case 4 => freqm(5)
//        case 5 => freqm(6)
//        case 6 => freqm(7) + freqm(0)
//        case 7 => freqm(8)
//        case 8 => freqm(0)
//      }
//    })
//    freqm.foreach(println)
//    println("***************")
//  }

  @tailrec
  def process(map: Map[Int, Long], day: Int): Long = {
//    println(map)

    if (day == 0) map.values.sum

    else process(map.map(k => {
        if(k._1 == 6) 6 -> (map(7) + map(0))
        else if(k._1 == 8) 8 -> map(0)
        else k._1 -> map(k._1 + 1)
      }), day - 1)
  }

  val i = process(freqm.toMap, 256)
//  2,3,2,0,1
//  After  2 days: 1,2,1,6,0,8
//  After  3 days: 0,1,0,5,6,7,8
//  4 days: 6,0,6,4,5,6,7,8,8
  println("final : " + i)
// non recursive

}
