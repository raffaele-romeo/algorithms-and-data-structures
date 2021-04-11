package search

object MaximumSubarraySum {

  def maximumSum(a: Array[Long], m: Long): Long = {
    a.toList.foldLeft((0L, 0L)) {
      case ((localMax, globalMax), elem) =>
        val newLocalMax = Math.max(elem, elem + localMax)
        (newLocalMax, if(Math.max(newLocalMax % m, elem % m) > globalMax) Math.max(newLocalMax % m, elem % m) else globalMax)
    }._2
  }

  def main(args: Array[String]): Unit = {
    val test = Array(1, 5, 9).map(_.toLong)

    println(maximumSum(test, 5))
  }

}
