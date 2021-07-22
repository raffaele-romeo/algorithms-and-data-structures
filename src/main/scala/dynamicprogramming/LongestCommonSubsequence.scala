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
        if(i == 0 || j == 0) {
          tableForLCS(i)(j) = 0
        } else if(s1.charAt(i - 1) == s2.charAt(j - 1)) {
          tableForLCS(i)(j) = tableForLCS(i - 1)(j - 1) + 1
        } else {
          tableForLCS(i)(j) = Math.max(tableForLCS(i - 1)(j), tableForLCS(i)(j - 1 ))
        }
    }
    tableForLCS(dimS1)(dimS2)
  }

  def main(args: Array[String]): Unit = {
    val s1 = "ABCD"
    val s2 = "ABDC"

    println(commonChild(s1, s2))

  }

}
