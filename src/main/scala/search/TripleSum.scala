package search

object TripleSum extends App {
  def triplets(a: Array[Int], b: Array[Int], c: Array[Int]): Long = {
    val aSet = a.distinct.sorted
    val bSet = b.distinct.sorted
    val cSet = c.distinct.sorted

    var count = 0L
    var m = 0
    var n = 0

    for (bElem <- bSet) {
      while (m < aSet.length && aSet(m) <= bElem) m += 1
      while (n < cSet.length && cSet(n) <= bElem) n += 1

      count += m * n
    }

    count
  }
}
