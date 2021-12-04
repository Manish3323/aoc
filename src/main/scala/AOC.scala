package com.example.aoc


abstract class AOC {
  def main(args: Array[String]): Unit = {
    println(part1(args(0)))
    println(part2(args(0)))
  }
  def part1(filename: String): Unit
  def part2(filename: String): Unit
}