package dynamicprogramming

object Abbreviation {

  def abbreviation(a: String, b: String): String = {
    var isItPossibleToBeEqual = true
    var i = 0
    var j = 0

    while (a.length >= b.length && isItPossibleToBeEqual && i < a.length && j < b.length) {
      if (a(i) == b(j)) {
        i += 1
        j += 1
      } else {
        if (Character.isUpperCase(a(i))) {
          isItPossibleToBeEqual = false
        } else if (i + 1 < a.length && a(i + 1) == b(j))
          i += 1
        else if(a(i).toUpper == b(j)) {
          i += 1
          j += 1
        } else {
          i += 1
        }
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
    val a = "gGIR"
    val b = "GIR"

    println(abbreviation(a, b))
  }

}
