package string

import scala.collection.mutable.{Map=> MMap}

object SherlockAndAnagrams {

  def sherlockAndAnagrams(s: String): Int = {
    val output = MMap.empty[Map[Char, Int], Int]

    for (i <- s.indices) {
      for (j <- i + 1 to s.length) {
        val subString = s.substring(i, j).toCharArray
        val frequencyCharForSubstring: Map[Char, Int] = getFrequencyMapFromArray(subString)
        output.addOne(frequencyCharForSubstring, output.getOrElse(frequencyCharForSubstring, 0) + 1)
      }
    }

      output.foldLeft(0){
        (sum, map) => sum + map._2 * (map._2 - 1)/2
      }
  }

  private def getFrequencyMapFromArray(arr: Array[Char]): Map[Char, Int] = {
    arr.foldLeft(Map[Char, Int]().empty) {
      case (map, next) => map + ((next, map.getOrElse(next, 0) + 1))
    }
  }

  def main(args: Array[String]): Unit = {
    val s = "ifai"

    println(sherlockAndAnagrams(s))

  }

}
