package nl.itjer.aoc2024

import nl.itjer.aoc.AOCUtils
import scala.collection.mutable.ArrayBuffer

object Day02:
  def getDirection(val1: Long, val2: Long): Int =
    if (val1 > val2)
      -1
    else
      1


  def filter(levels: Seq[Long], direction: Int): Boolean = {
    val newDirection = if (direction == 0)
      getDirection(levels(1), levels(2))
    else
      direction
    val result = levels.toList match {
      case head :: Nil => return true;
      case head :: tail => {
        if (1 to 3).contains ((tail(0)-head) * direction) then
          return filter(tail, newDirection);
        else 
          return false}
        case _ => {println(s"default");false}//  (1 to 3) contains (($head - ${tail(0)}) * $direction) ${ (1 to 3).contains ((head - tail(0)) * direction)}");false;}
    }
    //println(s"levels at end: ${levels.mkString(", ")} result $result");

    return result;
  }

  def reFilter(levels: Seq[Long]): Boolean = {
   levels.indices.exists(i => {
            val result = filter(ArrayBuffer(levels).remove(i), 0)
           // println(s"second chance: $i ${minOne.mkString(",")} $result")
            result  })
  }

  def main(args: Array[String]): Unit =
    val all = new AOCUtils().getFromAOC(2024, 2).split("\n")
    val sall = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
""".stripMargin.split("\n")
    println(s"part1: ${part1(all)}")
    println(s"solv2: ${solve2(all)}")
    println (s"part2 ${part2(all)}")


  def part1(lines: Seq[String]): String =
    lines.filter(report => { val zl = report.split(" ");
      val levels = zl.map(_.toLong);
      if (levels(0) == levels(1)) false;
      else
        val direction = if (levels(0) > levels(1)) -1 else 1
        filter(levels, direction)}).size.toString()


  def part2(lines: Seq[String]): String =
    val mineLines = lines.map(_.split(" ").map(_.toLong)).filter(levels => 
      if (levels(0) == levels(1)) false;
      else
        val direction = if (levels(0) > levels(1)) -1 else 1  
        filter(levels, direction) || reFilter(levels)

    ).map(_.mkString(","))
    //mineLines.foreach(ln => println("mine" + ln))
    val otherLines = lines.map(_.split(" ").map(_.toInt)).filter(report => isSafe(report) || safeEnough(report)).map(_.mkString(","))
    //otherLines.foreach(ln => println("other " + ln))
  
    otherLines.diff(mineLines).foreach(record => println(s"the diff ${record}"));
    ""
  
  def solve2(lines: Seq[String]): String = {
    val reports = lines.map(_.split(" ").map(_.toInt))

    val two = reports.filter(report => isSafe(report) || safeEnough(report)).size.toString()
    two
  }

  def safeEnough(report: Seq[Int]): Boolean = {
      report.indices
        .map { i =>
          report.slice(0, i) ++ report.slice(i + 1, report.size)
        }
        .exists(isSafe)
  } 
  private def isSafe(report: Seq[Int]) = {
    val pairs = report.sliding(2).toVector
    val diffs = pairs.map(p => p.head - p(1))

    val sameSign = diffs.forall(diff => diff.sign == diffs.head.sign)
    val small = diffs.forall(diff => diff.abs <= 3 && diff.abs >= 1)
    sameSign && small
  }

