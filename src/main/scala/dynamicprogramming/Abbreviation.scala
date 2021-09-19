package dynamicprogramming

object Abbreviation {

  def abbreviation(a: String, b: String): String = {
    var isItPossibleToBeEqual = true
    var i = 0
    var j = 0

    while (a.length >= b.length && isItPossibleToBeEqual && i < a.length && j < b.length) {
      if (a(i).toUpper == b(j)) {
        i += 1
        j += 1
      } else {
        if (Character.isUpperCase(a(i)))
          isItPossibleToBeEqual = false
        else
          i += 1
      }
    }

    while (i < a.length && isItPossibleToBeEqual) {
      if (Character.isUpperCase(a(i)))
        isItPossibleToBeEqual = false
      else
        i += 1
    }

    if (j == b.length && isItPossibleToBeEqual) {
      "YES"
    } else {
      "NO"
    }
  }

  def main(args: Array[String]): Unit = {
    val a = "ABCaaa"
    val b = ""

    println(abbreviation(a, b))
  }

}
