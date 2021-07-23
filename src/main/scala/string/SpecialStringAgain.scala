package string

object SpecialStringAgain {

  def substrCount(n: Int, s: String): Long = {
    val sDim = s.length
    var output = sDim
    var i = 0
    while (i < sDim) {
      var countSameElems = 0
      while (i + 1 < sDim && s.charAt(i) == s.charAt(i + 1)) {
        countSameElems += 1
        i += 1
      }
      output = output + (countSameElems * (countSameElems + 1)) / 2

      var pointer = 1
      while (i - pointer >= 0 && i + pointer < s.length && s.charAt(i + pointer) == s.charAt(i - 1) && s.charAt(i - pointer) == s.charAt(i - 1)) {
        output += 1
        pointer += 1
      }
      i += 1
    }
    output
  }

  def main(args: Array[String]): Unit = {
    val s1 = "aadaa"
    println(substrCount(5, s1))
  }

}
