package sorting

object FraudulentActivityNotifications {

  def activityNotifications(expenditure: Array[Int], d: Int): Int = {
    val count: Array[Int] = new Array[Int](201)
    var result: Int = 0

    for (i <- 0 until d) {
      count(expenditure(i)) += 1
    }

    for (i <- d until expenditure.length) {
      val median = getMedian(count)
      if (expenditure(i) >= 2 * median) {
        result += 1
      }
      count(expenditure(i - d)) -= 1
      count(expenditure(i)) += 1
    }
    result
  }

  def getMedian(count: Array[Int]): Double = {
    val sortedArray = count.foldLeft((Vector[Int](), 0)){
      case ((vector, index), elem) => (vector.++(List.fill(elem)(index)), index + 1)
    }._1

    if (sortedArray.length % 2 == 1) sortedArray(sortedArray.length / 2)
    else {
      val (up, down) = sortedArray.splitAt(sortedArray.length / 2)
      (up.last + down.head).toDouble / 2
    }
  }

  def main(args: Array[String]): Unit = {
    println(activityNotifications(Array(1, 2, 3, 4, 4), 4))
  }
}

