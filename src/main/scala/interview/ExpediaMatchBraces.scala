package interview

import scala.collection.mutable

object ExpediaMatchBraces {

  /*
   * Complete the 'braces' function below.
   *
   * The function is expected to return a STRING_ARRAY.
   * The function accepts STRING_ARRAY values as parameter.
   */

  def braces(values: Array[String]): Array[String] = {
    values.map(_.toCharArray).map(operateOnArray)
  }

  private def operateOnArray(array: Array[Char]): String = {
    val output = array.foldLeft(mutable.Stack[Char](), "YES") {
      case ((stack, output), elem) =>
        if (Set('{', '[', '(')(elem)) (stack.push(elem), output)
        else if (stack.nonEmpty && Set("{}", "()", "[]")(s"${stack.pop()}$elem")) (stack, output)
        else (stack, "NO")
    }

    if (output._1.nonEmpty) "NO"
    else output._2
  }

  def main(args: Array[String]): Unit = {
    val input = Array("{{()", "{[()]}", "{}[]()", "{[(])}")

    braces(input).foreach(println)
  }
}