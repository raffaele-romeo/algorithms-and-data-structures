package sorting

import scala.collection.mutable

object MissIntegers {

  def solution(a: Array[Int]): Int = {

    val output = a.foldLeft((new mutable.TreeSet[Int](), new mutable.TreeSet[Int](), 0)) {
      case ((testedSet, perfectSet, count), elem) => (testedSet.+=(elem), perfectSet, count + 1)
    }

    val testedSet = output._1
    val perfectSet = output._2

    perfectSet.find(elem => !testedSet.contains(elem)) match {
      case Some(value) => value
      case None => if (perfectSet.size == testedSet.size) perfectSet.size + 1
      else 1
    }
  }
}
