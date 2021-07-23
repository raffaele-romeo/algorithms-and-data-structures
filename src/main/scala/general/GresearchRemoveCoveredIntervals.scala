package general

object GresearchRemoveCoveredIntervals {

  def removeCoveredIntervals(intervals: Array[Array[Int]]): Int = {

    val sortedIntervals = intervals.sortWith {
      case (x: Array[Int], y: Array[Int]) =>
        if (x.head < y.head || (x.head == y.head && x(1) >= y(1))) true
        else false
    }

    if (sortedIntervals.length == 1) 0
    else
      sortedIntervals.length - sortedIntervals.tail.foldLeft((sortedIntervals.head(1), 0)) {
        case ((d, count), elem) =>
          val b = elem(1)
          if (b <= d)
            (d, count + 1)
          else
            (b, count)
      }._2
  }

  def main(args: Array[String]): Unit = {
    val test = Array(Array(1, 2), Array(1, 4), Array(3, 4))

    println(removeCoveredIntervals(test))
  }
}
