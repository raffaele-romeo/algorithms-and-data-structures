package graph

import scala.collection.mutable.{Queue => MQueue}
import scala.collection.mutable.{ListBuffer => MList}
import cats.implicits._


object AverageOfLevelsInBinaryTree {

  final case class Tree[A](value: A, left: Option[Tree[A]], right: Option[Tree[A]])

  object Tree {
    def buildOneElemTree[A](elem: A): Tree[A] = Tree(elem, None, None)

    def buildThreeElemTree[A](elem1: A, elem2: A, elem3: A): Tree[A] = Tree(elem1,
      buildOneElemTree(elem2).some,
      buildOneElemTree(elem3).some)
  }

  final case class Output(count: Int, sum: Double){
    def average: Double = if(count != 0) sum / count else 0
  }

  def averageOfLevels(root: Tree[Int]): List[Double] = {
    val q = new MQueue[Tree[Int]].enqueue(root)
    val output = MList[Double]()

    while (q.nonEmpty) {
      val tmpValue = q.indices.foldLeft(Output(0, 0.0)) {
        case (output, _) =>
          val qElem = q.dequeue()
          val elem = qElem.value

          if (qElem.right.isDefined)
            q.enqueue(qElem.right.get)

          if (qElem.left.isDefined)
            q.enqueue(qElem.left.get)

          Output(count = output.count + 1 , sum = output.sum + elem)
      }

      output.addOne(tmpValue.average)
    }

    output.toList
  }


  def main(args: Array[String]): Unit = {
    import Tree._
    val test1 = Tree(1,
      buildThreeElemTree(13, 20, 11).some,
      buildThreeElemTree(18, 35, 17).some)

    averageOfLevels(test1).foreach(println)

  }

}
