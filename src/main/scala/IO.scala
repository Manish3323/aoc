package com.example.aoc

import java.nio.file.{Files, Paths}

object IO:
  def readLines(resource: String): Array[String] = {
    val file = getClass.getClassLoader.getResource(resource).toURI
    Files.readString(Paths.get(file)).split("\n")
  }
  def readLines2(resource: String) = {
    val file = getClass.getClassLoader.getResource(resource).toURI
    Files.readString(Paths.get(file)).lines()
//      .split("\n")
  }
