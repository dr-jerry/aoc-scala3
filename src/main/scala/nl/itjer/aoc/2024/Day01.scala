package nl.itjer.aoc.`2024`

import nl.itjer.aoc.AOCUtils

class Day01
  val all = new AOCUtils().getFromAOC(2024, 1).split("\n")

  def parse(input: Seq[String]): Seq[(Long, Long)] =
    input.map(line => line match {
        case s"$left $right" => (left.trim().toLong, right.trim().toLong)
    })


  @main def run: Unit =
    println(s"part1: ${part1(all)}")
    println (s"part2 ${part2(all)}")


  def part1(lines: Seq[String]): String =
    val lr = parse(lines).unzip()
    return lr._1.sorted.zip(lr._2.sorted).map(t => Math.abs(t._1 - t._2)).sum().toString()

  def part2(lines: Seq[String]): String =
    val lr = parse(lines).unzip()
    lr._2.map(left => left * lr._1.count(_ == left)).sum().toString()

