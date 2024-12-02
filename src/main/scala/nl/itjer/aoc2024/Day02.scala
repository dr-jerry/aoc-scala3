package nl.itjer.aoc.`2024`

import nl.itjer.aoc.AOCUtils

class Day02
  val all2 = new AOCUtils().getFromAOC(2024, 1).split("\n")

  def filter(report: List[Long], direction: Int): Boolean = {
    return report match {
      case head :: tail => if 1 to 3 contains (head - tail(0) * direction) then
        return filter(tail, direction)
        else return false;
      case head :: Nil => return true;

    }
  }


  @main def run: Unit =
    println(s"part1: ${part1(all)}")
    println (s"part2 ${part2(all)}")


  def part1(lines: Seq[String]): String =
    all.filter(report => { val levels = report.split(" ").toList().map(_.toLong());
      if levels(0) == levels(1) then return false;
      else
        direction = if (levels(0) > level(1)) -1 else 1
        return filter(levels, direction)}).count().toString()

