package aoc2022

import com.example.aoc.IO.readLines

import scala.language.postfixOps

@main def Day1(): Unit = {
  var int = 1
  val map2 = readLines("2022/1.txt").foldLeft(Map.empty[Int, List[Int]]) { (map, d) => {
      d match {
        case _: String if d.isEmpty =>
          int += 1
          map
        case _: String if d.nonEmpty =>
          val list = map.getOrElse(int, List.empty[Int]) :+ d.toInt
          map ++ Map(int -> list)
      }
    }
  }
  val ss = map2.map(l => l._1 -> l._2.sum)
  // 1st question's answer
  println(ss.maxBy(d => d._2))

  val sss = ss.toSeq.sortWith(_._2 > _._2)
  // 2nd question's answer
  println(sss.head._2 + sss(1)._2 + sss(2)._2)
}