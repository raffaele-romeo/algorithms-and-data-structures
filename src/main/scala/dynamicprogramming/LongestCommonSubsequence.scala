package dynamicprogramming

object LongestCommonSubsequence {

  def commonChild(s1: String, s2: String): Int = {
    val dimS1 = s1.length
    val dimS2 = s2.length

    val tableForLCS = Array.ofDim[Int](dimS1 + 1, dimS2 + 1)

    for {
      i <- 0 to dimS1
      j <- 0 to dimS2
    } {
      if (i == 0 || j == 0) {
        tableForLCS(i)(j) = 0
      } else if (s1.charAt(i - 1) == s2.charAt(j - 1)) {
        tableForLCS(i)(j) = tableForLCS(i - 1)(j - 1) + 1
      } else {
        tableForLCS(i)(j) = Math.max(tableForLCS(i - 1)(j), tableForLCS(i)(j - 1))
      }
    }
    tableForLCS(dimS1)(dimS2)
  }

  //Slow solution
  def commonChildRecursive(s1: String, s2: String): String = {
    if (s1 == null || s1.isEmpty || s2 == null || s2.isEmpty) ""
    else if (s1 == s2) s1
    else {
      if (s1.head == s2.head) s1.head + commonChildRecursive(s1.substring(1), s2.substring(1))
      else {
        val lcs1 = commonChildRecursive(s1, s2.substring(1))
        val lcs2 = commonChildRecursive(s1.substring(1), s2)

        if (lcs1.length() > lcs2.length()) lcs1
        else lcs2
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val s1 = "ABCD"
    val s2 = "ABDC"

    println(commonChild(s1, s2))
    println(commonChildRecursive(s1, s2))

  }

}
