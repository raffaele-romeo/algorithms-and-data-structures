package sorting

object MergeSort {

  def mergeSortArray(array: Array[Int]): Array[Int] = {
    array.length match {
      case 0 | 1 => array
      case 2 => merge(array.slice(0, 1), array.slice(1, 2))
      case _ =>
        val mid = array.length / 2 + 1

        val firstHalf = mergeSortArray(array.slice(0, mid))
        val secondHalf = mergeSortArray(array.slice(mid, array.length))

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


  def main(args: Array[String]): Unit = {
    def mergeSort(input: List[Int]): List[Int] = mergeSortArray(input.toArray).toList

    assert(mergeSort(List.empty) == List.empty)
    assert(mergeSort(List(1)) == List(1))
    assert(mergeSort(List(1, 2)) == List(1, 2))
    assert(mergeSort(List(2, 1)) == List(1, 2))
    assert(mergeSort(List(2, 1, 3)) == List(1, 2, 3))
    assert(mergeSort(List(2, 1, 4, 3)) == List(1, 2, 3, 4))
    assert(mergeSort(List(2, 4, 5, 1, 3)) == List(1, 2, 3, 4, 5))
    assert(
      {
        val randomArray = scala.util.Random
          .nextBytes(10 + Math.abs(scala.util.Random.nextInt(1000)))
          .map(_.toInt)
          .toList
        mergeSort(randomArray) == randomArray.sorted
      },
      "Random array of any length is sorted"
    )

  }

}
