package general

import scala.annotation.tailrec

object WilliamHillTask1 {

  def solution(s: String): Int = {
    @tailrec
    def innerMethod(s: List[Int], max: Int): Int = {
      s match {
        case Nil => max
        case _ :: Nil => max
        case ::(head, tl) =>
          val number: String = s"$head${tl.head}"
          innerMethod(tl, Math.max(max, number.toInt))
      }
    }

    innerMethod(s.map(_.asDigit).toList, 0)
  }

  def main(args: Array[String]): Unit = {
    val test1 = "50552"

    println(solution(test1))
  }

}
