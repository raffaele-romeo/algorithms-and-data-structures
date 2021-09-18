package search

object Pairs {

  def pairs(k: Int, arr: Array[Int]): Int = {
    var count = 0
    val sortedArray = arr.sorted
    var i = 0
    var j = 1

    while (i < arr.length  & j < arr.length) {
      while(j < (arr.length - 1) & sortedArray(j) - sortedArray(i) < k) j += 1

      if (sortedArray(j) - sortedArray(i) == k) {
        count += 1
        j += 1
        i += 1
      } else
        i += 1
    }

    count
  }

  def main(args: Array[String]): Unit = {
    val k = 2
    val input = Array(1, 3, 5, 8, 6, 4, 2)

    println(pairs(2, input))
  }

}
