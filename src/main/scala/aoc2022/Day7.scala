package com.example.aoc
package aoc2022

import IO.{readLines, readLines2}

import scala.collection.immutable.{Map, TreeSet}
import scala.collection.mutable
import scala.language.postfixOps


sealed trait FS

case class File(value: String, size: Long) extends FS

case class Directory(name: String, files: List[File], parent: Option[String]) extends FS

@main def Day7(): Unit = {
  val lines = readLines("2022/7.txt")
  var pwd = ""
  var parent = Array.empty[String]
  var fs = Map[String, (List[Directory], List[File], Long)]("/" -> (List.empty[Directory], List.empty[File], 0L))

  def createTree(lines: Array[String]): Unit =
    lines.foreach {
      case s"$$ ls" => ;
      case s"$$ cd ${cmd}" =>
        cmd match
          case s".." =>
            pwd = parent.last
            parent = parent.take(parent.length - 1)
          case s"$cd" =>
            parent = parent.appended(pwd)
            pwd = cd
      case s"dir ${dir}" =>
        val path = parent.mkString + pwd
        val currentItems = fs.getOrElse(path, (List.empty[Directory], List.empty[File], 0L))
        val newDir = List(Directory(path + dir, List.empty[File], Some(pwd)))
        fs = fs ++ Map(path -> (currentItems._1 ++ newDir, currentItems._2, 0L))
      case s"$size $file" =>
        val path = parent.mkString + pwd
        val currentItems = fs.getOrElse(path, (List.empty[Directory], List.empty[File], 0L))
        val files = currentItems._2 ++ List(File(file, size.toLong))
        fs = fs ++ Map(path -> (currentItems._1, files, 0L))
    }

  def readSize(dirName: String, tobeVisited: Set[String]): (String, Long) = {
    val tuple = fs(dirName)
    val sum = tuple._1.map(d => {
      tobeVisited.find(_ == d.name) match
        case Some(value) =>
          readSize(value, tobeVisited.filterNot(_ == d.name))._2
        case None => d.files.map(_.size).sum
    }).sum + tuple._2.map(_.size).sum
    val item = fs(dirName)
    fs = fs ++ Map(dirName -> (item._1, item._2, sum))
    (dirName,sum)
  }

  createTree(lines)
  val outermostSize = readSize("/", fs.keySet)

  val minimumReq = 30000000 - 70000000 + outermostSize._2
  val l = fs.filter(_._1 != "/").foldLeft(Long.MaxValue)((min, dir) => {
    if dir._2._3 >= minimumReq then {
      Math.min(dir._2._3, min)
    }
    else min
  })
  println(l)
  //  57245837
  //  57245837
  //  57073053
  //  57073053
}