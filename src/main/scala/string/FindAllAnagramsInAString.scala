package string

import scala.collection.mutable.{Map => MMap, ListBuffer => MList}

object FindAllAnagramsInAString {

  //Given two strings s and p, return an array of all the start indices of p's anagrams in s. You may return the answer in any order.
  def findAnagrams(bigString: String, smallString: String): List[Int] = {
    val result = MList.empty[Int]
    val smallStringLength = smallString.length
    val bigStringLength = bigString.length

    if(smallStringLength <= bigStringLength) {
      val smallStringFrequencyCount = smallString.foldLeft(Map[Char, Int]().empty) {
        case (map, next) => map + ((next, map.getOrElse(next, 0) + 1))
      }

      val bigStringFrequencyCount = MMap[Char, Int]().empty

      for (i <- 0 until smallStringLength) {
        val bigStringChar = bigString(i)

        if (smallStringFrequencyCount.contains(bigStringChar))
          bigStringFrequencyCount.addOne((bigStringChar, bigStringFrequencyCount.getOrElse(bigStringChar, 0) + 1))
      }

      if (smallStringFrequencyCount.equals(bigStringFrequencyCount))
        result.addOne(0)

      for (i <- 0 until bigStringLength - smallStringLength) {
        val leftCharacter = bigString(i)
        val rightCharacter = bigString(i + smallStringLength)

        if (smallStringFrequencyCount.contains(rightCharacter))
          bigStringFrequencyCount.addOne((rightCharacter, bigStringFrequencyCount.getOrElse(rightCharacter, 0) + 1))

        if (smallStringFrequencyCount.contains(leftCharacter))
          bigStringFrequencyCount.addOne((leftCharacter, bigStringFrequencyCount.getOrElse(leftCharacter, 0) - 1))

        if (smallStringFrequencyCount.equals(bigStringFrequencyCount))
          result.addOne(i + 1)
      }
    }

    result.toList
  }

  def main(args: Array[String]): Unit = {
    val bigString = "bpaa"
    val smallString = "aa"

    println(findAnagrams(bigString, smallString))

  }

}
