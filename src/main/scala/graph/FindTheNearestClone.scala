package graph

import scala.collection.mutable.{Map => MMap, Queue => MQueue, ListBuffer => MList}

object FindTheNearestClone {

  def findShortest(graphNodes: Int, graphFrom: Array[Int], graphTo: Array[Int], ids: Array[Long], value: Int): Int = {

    type Node = Int
    type Graph = MMap[Node, List[Node]]

    val graph: Graph = MMap.empty[Node, List[Node]]

    def addElementToMap(key: Int, value: Int): Graph = {
      graph.addOne((key, value :: graph.getOrElse(key, List.empty[Node])))
    }

    for (i <- graphFrom.indices) {
      addElementToMap(graphFrom(i), graphTo(i))
      addElementToMap(graphTo(i), graphFrom(i))
    }

    val elementsWithSameColor = for {
      i <- ids.indices if ids(i) == value
    } yield i + 1


    if (elementsWithSameColor.size < 2)
      -1
    else {
      val startNode = elementsWithSameColor.head
      val otherNodes = elementsWithSameColor.tail.toList

      val queue = MQueue(startNode)
      val visited = MList(startNode)
      var output = 0
      var foundValue = false

      while (queue.nonEmpty && !foundValue) {
        val n = queue.dequeue()

        val connectedNodes = graph(n)

        connectedNodes.foreach { node =>
          if (otherNodes.contains(node)) {
            foundValue = true
          } else {
            if (!visited.contains(node)) {
              queue.enqueue(node)
              visited.addOne(node)
              output = output + 1
            }
          }
        }
      }
      output
    }
  }

  def main(args: Array[String]): Unit = {
    val output = findShortest(
      5,
      Array(1,1,2,3),
      Array(2,3,4,5),
      Array(1,2,3,3,2),
      value = 2)


    println(output)
  }
}
