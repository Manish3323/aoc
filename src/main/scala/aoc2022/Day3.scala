package com.example.aoc
package aoc2022

import IO.readLines

import scala.language.postfixOps

@main def Day3(): Unit = {
    // problem1
    val sacks1 = readLines("2022/3.txt").map(d => {
      val tuple = d.splitAt(d.length / 2)
      Seq(tuple._1, tuple._2)
    }).iterator

    //problem 2
    val sacks2 = readLines("2022/3.txt").sliding(3,3).map(d => {
      d.toSeq
    })

    def getValue(char: Char): Int = {
      val asciiCode = char.toInt
      if(asciiCode >= 65 && asciiCode <= 90) {
        //uppercase items
        asciiCode - 38
      }else {
        //lowercase item
        asciiCode - 96
      }
    }

    def problem(input: Iterator[Seq[String]]): Unit = {
      val sum = input.map(sackGroup =>
        sackGroup.map(_.toCharArray)
      ).map(sack => {
        //main logic which checks a char from first Array exists in all other arrays for that sack group
        val charExistsInAllSacks = sack.head.filter(char => sack.tail.exists(_.contains(char)))(0)
        getValue(charExistsInAllSacks)
      }).sum
      println(sum)
    }

    problem(sacks1)
    problem(sacks2)
}