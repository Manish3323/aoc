

package com.example.aoc
package aoc2021
import IO._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day17 extends App {
  type Point = (Int, Int)
  type Velocity = (Int, Int)
  type IsInTargetArea = Boolean
  type MaxHeight = Int
  val targetArea = readLines("17.txt").head match {
    case s"target area: x=$minx..$maxx, y=$miny..$maxy" => (Range.inclusive(minx.toInt, maxx.toInt), Range.inclusive(miny.toInt, maxy.toInt))
  }

  def isWithinTarget(p: Point) = targetArea._1.contains(p._1) && targetArea._2.contains(p._2)

  def updateVel(v: Velocity): Velocity = {
    val nextXVel = v._1 match {
      case 0  => 0
      case x if x < 0 => x + 1
      case x if x > 0 => x - 1
    }
    (nextXVel, v._2 - 1)
  }

  def hasCrossedTarget(p: Point) = p._1 > targetArea._1.last || p._2 < targetArea._2.head

  def nextStep(p: Point, currentVel: Velocity): Point = (p._1 + currentVel._1, p._2 + currentVel._2)

  @tailrec
  def step(p: Point, velocity: Velocity, initialVelocity: Velocity, highestY: MaxHeight): (Velocity, IsInTargetArea, MaxHeight) = {
    val nextPosition = nextStep(p, velocity)
    val nextVelocity = updateVel(velocity)
    val newHighest = highestY max nextPosition._2
    if(!hasCrossedTarget(nextPosition)){
      if(isWithinTarget(nextPosition)){
        (initialVelocity, true, newHighest)
      }else{
        step(nextPosition, nextVelocity, initialVelocity, newHighest)
      }
    }else{
      (initialVelocity, false, newHighest)
    }
  }


  def calculateNext(): Unit = {
    var maxH = 0
    var maxV = (0,0)
    val noOfVelocitiesThatHit = ArrayBuffer.empty[Point]
    val velXRange = Range.inclusive(1, targetArea._1.last)
    val velYRange = Range.inclusive(-1000, 1000)

    for {
      vx <- velXRange
      vy <- velYRange
    } yield {
      val tuple = step((0, 0), (vx,vy), (vx ,vy), 0)
      if(tuple._2) {
        noOfVelocitiesThatHit.addOne(tuple._1)
        if(maxH < tuple._3) maxV = tuple._1
        maxH = tuple._3 max maxH
      }
    }

    println("part1 : " + maxH)
    println("part2 : " + noOfVelocitiesThatHit.length)
  }
  calculateNext()
}
