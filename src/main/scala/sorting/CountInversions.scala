package sorting

object CountInversions {
  def countInversions(arr: Array[Int]): Long = {
    // Write your code here
    var count = 0

    def mergeSortArray(array: Array[Int]): Array[Int] = {
      val mid = array.length / 2

      if (mid == 0) array
      else {
        val splitList = array.splitAt(mid)

        val firstHalf = mergeSortArray(splitList._1)
        val secondHalf = mergeSortArray(splitList._2)

        merge(firstHalf, secondHalf)
      }
    }

    def merge(array1: Array[Int], array2: Array[Int]): Array[Int] = {
      val dimArray1 = array1.length
      val dimArray2 = array2.length

      val array = Array.ofDim[Int](dimArray1 + dimArray2)

      var i = 0
      var j = 0
      var k = 0

      while (i < dimArray1 && j < dimArray2) {
        if(array1(i) <= array2(j)) {
          array(k) = array1(i)
          i += 1
        } else {
          array(k) = array2(j)
          count += dimArray1 - i
          j += 1
        }
        k += 1
      }

      while(i < dimArray1) {
        array(k) = array1(i)
        i += 1
        k += 1
      }

      while(j < dimArray2) {
        array(k) = array2(j)
        j += 1
        k += 1
      }

      array
    }

    mergeSortArray(arr)
    count
  }


  def main(args: Array[String]): Unit = {
    val array = Array(1, 1, 1, 2, 2)

    println(countInversions(array))

  }

}
