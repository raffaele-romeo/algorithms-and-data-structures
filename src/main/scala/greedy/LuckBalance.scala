package greedy

import scala.collection.mutable

object LuckBalance {

  def luckBalance(k: Int, contests: Array[Array[Int]]): Int = {
    val priorityQueue = mutable.PriorityQueue.empty[Int]
    var luckBalance = 0

      for (i <- contests.indices) {
        val luckImportance = contests(i)
        val luck = luckImportance.head
        val importance = luckImportance(1)

        if(importance == 0)
          luckBalance += luck
        else priorityQueue.addOne(luck)
      }

    val dequeueAll = priorityQueue.dequeueAll
    val toBeSub = dequeueAll.take(k)

    luckBalance + 2 * toBeSub.sum - dequeueAll.sum
  }

  def main(args: Array[String]): Unit = {
    val input = Array(Array(5,1), Array(2,1), Array(1,1), Array(8,1), Array(10,0), Array(5,0))

    println(luckBalance(3, input))

  }

}
