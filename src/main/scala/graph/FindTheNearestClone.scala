package graph

object FindTheNearestClone {

  def findShortest(graphNodes: Int, graphFrom: Array[Int], graphTo: Array[Int], ids: Array[Long], value: Int): Int = {

    val elementsWithSameColor = for {
      i <- ids.indices if(ids(i) == value)
    } yield i + 1




  }


}
