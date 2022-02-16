package string

import scala.collection.mutable.{ListBuffer => MList}

object SubstringOfAString {

  def subStrings(s: String): List[String] = {
    val output = MList.empty[String]
    for (i <- s.indices) {
      for (j <- i + 1 to s.length) {
        output.addOne(s.substring(i, j))
      }
    }
    output.toList
  }

  def main(args: Array[String]): Unit = {
    val s = "abcd"

    println(subStrings(s))
  }

}
