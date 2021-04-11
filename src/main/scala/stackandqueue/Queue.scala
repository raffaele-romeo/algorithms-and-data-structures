package stackandqueue

import stackandqueue.Queue.EmptyQueue

sealed trait Queue[+T] {
  def enqueue[A >: T](element: A): Queue[A]
  def dequeue[A >: T]: Queue[A]
  def peek[A >: T](): Queue[A]
}

object Queue {
  case class NonEmptyQueue[+T](in: Vector[T]) extends Queue[T] {
    override def enqueue[A >: T](element: A): Queue[A] = NonEmptyQueue[A](in :+ element)

    override def dequeue[A >: T]: Queue[A] = NonEmptyQueue[T](in.tail)

    override def peek[A >: T](): Queue[A] = {
      println(in.head)
      this
    }
  }

  case class EmptyQueue[+T]() extends Queue[T] {
    override def enqueue[A >: T](element: A): Queue[A] = NonEmptyQueue[A](Vector[A](element))

    override def dequeue[A >: T]: Queue[A] = throw new NoSuchElementException("Dequeue on Empty Queue")

    override def peek[A >: T](): Queue[A] = throw new NoSuchElementException("Peek on Empty Queue")
  }

  def apply[T](): Queue[T] = EmptyQueue[T]()
  def apply[T](elem: T): Queue[T] = NonEmptyQueue[T](Vector[T](elem))
}

object CustomQueue {
  def main(args: Array[String]): Unit = {
    val emptyQueue = Queue[Int](4)

    val output = emptyQueue.enqueue(5).enqueue(6).enqueue(7).dequeue.enqueue(4)

    println(output.toString)
  }

}
