package nl.itjer.aoc2024

import nl.itjer.aoc.AOCUtils

object Day04:

  var directions = List(Vector (
    (1,1, 'M'),(2,2, 'A'),(3,3, 'S'))
    ,Vector ((-1,-1, 'M'),(-2,-2, 'A'),(-3,-3, 'S'))
    ,Vector ((-1,1, 'M'),(-2,2, 'A'),(-3,3, 'S'))
    ,Vector ((1,-1, 'M'),(2,-2, 'A'),(3,-3, 'S'))
    ,Vector ((1,0, 'M'),(2,0, 'A'),(3,0, 'S'))
    ,Vector ((-1,0, 'M'),(-2,0, 'A'),(-3,0, 'S'))
    ,Vector ((0,1, 'M'),(0,2, 'A'),(0,3, 'S'))
    ,Vector ((0,-1, 'M'),(0,-2, 'A'),(0,-3, 'S')));

  var xdirections = List(
    Vector ((-1,-1, 'M'),(1,1, 'S'),(-1,1, 'M'),(1,-1, 'S'))
    ,Vector ((-1,-1, 'S'),(1,1, 'M'),(-1,1, 'M'),(1,-1, 'S'))
    ,Vector ((-1,-1, 'S'),(1,1, 'M'),(-1,1, 'S'),(1,-1, 'M'))   
    ,Vector ((-1,-1, 'M'),(1,1, 'S'),(-1,1, 'S'),(1,-1, 'M'))
  )

  def main(args: Array[String]): Unit =
    val all = new AOCUtils().getFromAOC(2024, 4).split("\n")
    val sall = """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX""".split("\n")
    println(s"part1: ${part1(all, 'X', directions)}")
    println (s"part2 ${part1(all, 'A', xdirections)}")


  def part1(lines: Array[String], startChar: Char, directs: List[Vector[(Int, Int, Char)]]): String =
    (lines.indices.map(y => { lines(y).zipWithIndex.filter(tup => tup._1 == startChar)
        .map(tup => tup match {
            case (_, x) => {
            directs.count(vec => vec.forall( delta => try { 
              lines(y+delta._2)(x+delta._1) == delta._3 
            } catch _ => false)
        )}}).sum
      })).sum.toString()


