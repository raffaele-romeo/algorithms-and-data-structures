package interview

import scala.util.Try

object WilliamHillTask2 {
  val SpecialCharacter = '#'

  def solution(s: String, t: String): Boolean = {

    val transformedS = transformInput(s).flatMap(addPlaceholder)
    val transformedT = transformInput(t).flatMap(addPlaceholder)

    if (transformedS.size != transformedT.size)
      false
    else {
      val findPairs = transformedS.zip(transformedT).find {
        case (c, c1) => c != c1 & c != SpecialCharacter & c1 != SpecialCharacter
      }

      findPairs.isEmpty
    }
  }

  private def transformInput(s: String): List[String] = {
    val output = s.toCharArray.foldLeft((Vector[String](), "")) {
      case ((transformedElement, runningDigit), elem) =>
        if (elem.isDigit) (transformedElement, s"$runningDigit${elem.asDigit}")
        else {
          if (runningDigit.nonEmpty) (transformedElement.:+(runningDigit).:+(elem.toString), "")
          else (transformedElement.:+(elem.toString), "")
        }
    }

    if (output._2.nonEmpty) output._1.:+(output._2).toList
    else output._1.toList
  }

  private def addPlaceholder(s: String): List[Char] = {
    if (Try(s.toInt).isSuccess) List.fill(s.toInt)(SpecialCharacter)
    else List(s.charAt(0))
  }

  def main(args: Array[String]): Unit = {
    val s = "150c150s"
    val t = "150c150S"

    println(solution(s, t))
  }

}
