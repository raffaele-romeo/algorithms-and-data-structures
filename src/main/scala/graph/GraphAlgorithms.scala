package graph

import scala.collection.mutable.{ListBuffer => MList, Queue => MQueue, Stack => MStack}

object GraphAlgorithms {

  type Node = Int
  type Graph = Map[Node, List[Node]]

  //Breadth First Search
  def bfs(graph: Graph, node: Node): List[Node] = {
    require(graph.keySet(node))

    val queue = MQueue(node)
    val output = MList(node)

    while (queue.nonEmpty) {
      val n = queue.dequeue()

      val connectedNodes = graph(n)

      connectedNodes.foreach { node =>
        if (!output.contains(node)) {
          queue.enqueue(node)
          output.addOne(node)
        }
      }
    }

    output.toList
  }

  //Depth First Search
  def dfs(graph: Graph, node: Node): List[Node] = {
    require(graph.keySet(node))

    val stack = MStack(node)
    val output = MList.empty[Node]

    while (stack.nonEmpty) {
      val topNode = stack.top
      stack.pop()

      if (!output.contains(topNode)) {
        output.addOne(topNode)
      }

      val connectedNodes = graph(topNode)
      connectedNodes.foreach { node =>
        if (!output.contains(node)) {
          stack.push(node)
        }
      }
    }
    output.toList
  }

  def main(args: Array[String]): Unit = {
    val graph: Map[Node, List[Node]] = List(
      (1, List(4, 3, 2)),
      (4, List(1, 3)),
      (2, List(1, 3, 5, 7, 8)),
      (3, List(4, 2, 10, 9)),
      (10, List(3)),
      (5, List(6, 2, 7, 8)),
      (6, List(5)),
      (7, List(5, 8, 2)),
      (8, List(2, 5, 7)),
      (9, List(3)),
    ).toMap

    val dfsOutput = dfs(graph, 9)
    val bfsOutput = bfs(graph, 9)

    println(dfsOutput.mkString(", "))
    println(dfsOutput.size)

    println(bfsOutput.mkString(", "))
    println(bfsOutput.size)

  }

}
