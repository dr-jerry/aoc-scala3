package nl.itjer.aoc.`2015`

import nl.itjer.aoc.AOCUtils

class Day01
  val all = new AOCUtils().getFromAOC(2015, 1);
  @main def run: Unit =
    part1()
    println (s"part2 ${part2()._1}")


  def part1(): Unit =
    println(s"count ${all.count(_ == '(') - all.count(_ == ')')}")

  def part2(): (Int, Int) =
    all.foldLeft(1,0)((tup: (Int, Int), c: Char) => 
        if (c == '(')
            (tup._1 + 1, (tup._2) +1)
        else 
            if (c == ')')
                if (tup._2 == 0) 
                    println(s"reached attic @$tup")
                    return tup
                else 
                    (tup._1 + 1, tup._2 -1 )
            else
                tup
       
    )

