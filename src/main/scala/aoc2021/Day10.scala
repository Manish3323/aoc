package com.example.aoc.aoc2021

import scala.collection.mutable
import com.example.aoc.IO

@main
def Day10(): Unit = {
  val input = IO.readLines("10.txt")
  val valueMap = Map('}'-> 1197, ']' -> 57, '>' -> 25137, ')' ->3)
  val charMap: Map[Char,Char] = Map(
    '{' -> '}',
    '[' -> ']',
    '<' -> '>',
    '(' -> ')'
  )


//part1
//  val lines: Array[Char] = input.map(line => {
//    val stack = new mutable.Stack[Char]()
//    var corrupt = '.'
//    for {
//      char <- line if corrupt == '.'
//    } do {
//      char match {
//        case char@'{' => stack.addOne(char)
//        case char@'<' => stack.addOne(char)
//        case char@'(' => stack.addOne(char)
//        case char@'[' => stack.addOne(char)
//        case char@']' => {
//          if(stack.last == '[')
//            stack.removeLast()
//          else{
//            corrupt = ']'
//          }
//        }
//        case char@'}' =>
//          if(stack.last == '{')
//            stack.removeLast()
//          else{
//            //            println(s"Expected ${charMap(stack.last)}, but found "+ '}' + " instead.")
//            corrupt = '}'
//          }
//        case char@')' =>
//          if(stack.last == '(')
//            stack.removeLast()
//          else{
//            corrupt = ')'
//          }
//        case char@'>' =>
//          if(stack.last == '<')
//            stack.removeLast()
//          else{
//            corrupt = '>'
//          }
//      }
//    }
//    if(corrupt == '.')
//  })
//  val part1ans = lines.filter(x => x != '.').map(x =>valueMap(x)).sum
  val incompleteLines: Array[(String, (mutable.Stack[Char], Boolean))] = input.map(line => {
    val stack = new mutable.Stack[Char]()
    var corrupt = '.'
    for {
      char <- line if corrupt == '.'
    } do {
      char match {
        case char@'{' => stack.addOne(char)
        case char@'<' => stack.addOne(char)
        case char@'(' => stack.addOne(char)
        case char@'[' => stack.addOne(char)
        case char@']' => {
          if(stack.last == '[')
            stack.removeLast()
          else{
            corrupt = ']'
          }
        }
        case char@'}' =>
          if(stack.last == '{')
            stack.removeLast()
          else{
            corrupt = '}'
          }
        case char@')' =>
          if(stack.last == '(')
            stack.removeLast()
          else{
            corrupt = ')'
          }
        case char@'>' =>
          if(stack.last == '<')
            stack.removeLast()
          else{
            corrupt = '>'
          }
      }
    }
    if(corrupt != '.') line -> (stack, true)
    else if(stack.isEmpty) line -> (stack, true)
    else line -> (stack, false)
  })

  val valueMap2 = Map('}'-> 3, ']' -> 2, '>' -> 4, ')' ->1)
  val sorted = incompleteLines.filterNot(x => x._2._2).map(x => x._2._1).map(x => {
    val reverse = x.reverse
    reverse.foldLeft(0L)((score, char) => {
      (score * 5) + valueMap2(charMap(char))
    })
  }).sorted
  println(sorted(sorted.length/2))

}
