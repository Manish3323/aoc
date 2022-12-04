package com.example.aoc
package aoc2022

import IO.readLines

import scala.language.postfixOps

@main def Day4(): Unit = {
    // problem1
    def makeList(start: Int, end: Int) = List.range(start, end + 1)
    val pairs = readLines("2022/4.txt").map(_.split(",")).map(pair => {
        val firstTuple = pair.head.split("-")
        val secondTuple = pair.last.split("-")
        (makeList(firstTuple.head.toInt, firstTuple.last.toInt), makeList(secondTuple.head.toInt, secondTuple.last.toInt))
    })
    val booleans1 = pairs.count(pair => {
        val list1 = pair._1
        val list2 = pair._2
        (list2.contains(list1.head)) && (list2.contains(list1.last)) ||
        (list1.contains(list2.head)) && (list1.contains(list2.last))
    })
    val booleans2 = pairs.count(pair => {
        val list1 = pair._1
        val list2 = pair._2
        list1.exists(list2.contains(_)) || list2.exists(list1.contains(_))
    })
    println("answer 1 : " + booleans1)
    println("answer 2 : " + booleans2)
}