package nl.itjer.aoc2024

import nl.itjer.aoc.AOCUtils

object Day03:


  def main(args: Array[String]): Unit =
    val all = new AOCUtils().getFromAOC(2024, 3)
    val all1 = "mul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    val all2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    println(s"part1: ${part1(all)}")
    println (s"part2 ${part2(all)}")


  def part1(instructions: String): String =
    var pat = """mul\((\d+),(\d+)\)""".r
    var result = 0L;
    for mat <- pat.findAllMatchIn(instructions) do
      println(mat.matched)
      result += mat.group(1).toLong * mat.group(2).toLong
    return result.toString()
    instructions.split("mull(\\d+,\\d+)").toList.filter(ml => {println("ml : " + ml);
     ml.startsWith("mul")}).map(mul => mul match {
      case s"mul($left,$right)" => left.toLong * right.toLong}).sum.toString()
    
  def part2(instructions: String): String =
    var result = 0L
    var pat = """(mul\(\d+,\d+\))|(do\(\))|(don't\(\))""".r
    var doit = true
    for mat <- pat.findAllMatchIn(instructions) do
      println("match " + mat.matched)
      mat.matched match {
        case s"mul($left,$right)" if doit => result += left.toLong * right.toLong;
        case s"do()" => doit = true
        case s"don't()" => doit = false
        case _ => ()
      }
    return result.toString()



