package com.example.aoc

@main def Day2(): Unit = {
  val input = IO.readLines("2.txt")
//  val commands = input.map(x => x.split(" ").toList match {
//    case Nil => ???
//    case command :: value => (command, value.head.toInt)
//  })

  val commands = input.map(x => {
    val parts = x.split(" ").toList
    (parts.head, parts.last.toInt)
  })
//  // 1
//  var (horizontal, depth, aim) = (0,0)
//  commands.foreach((c) =>
//    if(c._1 == "forward")  horizontal = horizontal + c._2
//    if(c._1 == "down")  depth = depth + c._2
//    if(c._1 == "up")  depth = depth - c._2
//  )
    // 2
  var (horizontal, depth, aim) = (0, 0, 0)

  commands.foreach {
    case ("forward", value) => {
      horizontal = horizontal + value
      depth = depth + aim * value
    }
    case ("down", value) => aim = aim + value
    case ("up", value) => aim = aim - value
  }

  print(horizontal * depth)
}
