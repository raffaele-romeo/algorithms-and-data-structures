package hashmap

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object CountTriplets {

  def countTriplets(arr: Array[Long], r: Long): Long = {
    @tailrec
    def loop(list: List[Long], leftMap: Map[Long, Long], rightMap: Map[Long, Long], output: Long): Long = {
      list match {
        case ::(head, next) =>
          val newRightMap = updateElementFromFrequencyMap(head, rightMap, _ - _)
          val keyToSearchOnLeftMap = head / r
          val keyToSearchOnRightMap = head * r

          val m = if (leftMap.contains(keyToSearchOnLeftMap) && head % r == 0) leftMap(keyToSearchOnLeftMap) else 0
          val n = newRightMap.getOrElse(keyToSearchOnRightMap, 0L)
          val numberOfTriplets = m * n

          loop(
            next,
            updateElementFromFrequencyMap(head, leftMap, _ + _),
            newRightMap,
            output + numberOfTriplets
          )
        case Nil => output
      }
    }

    loop(arr.toList, Map[Long, Long]().empty, getFrequencyMapFromArray(arr), 0L)
  }

  private def updateElementFromFrequencyMap(element: Long, map: Map[Long, Long], operator: (Long, Long) => Long): Map[Long, Long] = {
    map + ((element, operator(map.getOrElse(element, 0L), 1L)))
  }

  private def getFrequencyMapFromArray(arr: Array[Long]): Map[Long, Long] = {
    arr.foldLeft(HashMap[Long, Long]().empty) {
      case (map, next) => map + ((next, map.getOrElse(next, 0L) + 1L))
    }
  }

  def main(args: Array[String]): Unit = {
    val r = 1
    val example = List.fill(100)(1).toArray.map(_.toLong)

    println(countTriplets(example, r))
  }

}
