package graph

import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer => MList, Map => MMap, Queue => MQueue}

object FindTheNearestClone {

  type Node = Int
  type Graph = Map[Node, List[Node]]

  def addNodesToGraph(graph: Graph, graphFrom: Array[Int], graphTo: Array[Int]): Graph = {
    @tailrec
    def loop(graph: Graph, edges: List[(Node, Node)]): Graph = {
      edges match {
        case Nil => graph
        case head :: tail =>
          val newGraph = addNode(head._1, head._2).compose(addNode(head._2, head._1))(graph)
          loop(newGraph, tail)
      }
    }

    def addNode(node1: Node, node2: Node): Graph => Graph =
      graph => graph + ((node1, node2 :: graph.getOrElse(node1, List.empty[Node])))

    val edges: Array[(Node, Node)] = graphFrom.zip(graphTo)
    loop(graph, edges.toList)
  }

  def findShortest(graphNodes: Int, graphFrom: Array[Int], graphTo: Array[Int], ids: Array[Long], value: Int): Int = {

    val graph = addNodesToGraph(Map.empty[Node, List[Node]], graphFrom, graphTo)

    val nodesWithSameColourAsValue = for {
      i <- ids.indices if ids(i) == value
    } yield i + 1


    if (nodesWithSameColourAsValue.size >= 2) {
      val startNode = nodesWithSameColourAsValue.head
      val otherNodes = nodesWithSameColourAsValue.tail.toList

      val queue = MQueue(startNode)
      val visited = MList(startNode)
      var output = 0
      var isNearestClone = false

      while (queue.nonEmpty && !isNearestClone) {
        val n = queue.dequeue()
        val connectedNodes = graph(n)

        connectedNodes.foreach { node =>
          if (otherNodes.contains(node))
            isNearestClone = true
          else if (!visited.contains(node)) {
            queue.enqueue(node)
            visited.addOne(node)
            output += 1
          }
        }
      }
      output
    } else -1
  }

  def main(args: Array[String]): Unit = {
    val output = findShortest(
      5,
      Array(1, 1, 2, 3),
      Array(2, 3, 4, 5),
      Array(1, 2, 3, 3, 2),
      value = 2)


    println(output)
  }
}
