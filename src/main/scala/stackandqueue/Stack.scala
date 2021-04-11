package stackandqueue

import stackandqueue.Stack.EmptyStack

sealed trait Stack[+T] {
  def push[A >: T](element: A): Stack[A]
  def pop[A >: T]: Stack[A]
  def peek[A >: T](): Stack[A]
}

object Stack {

  case class NonEmptyStack[+T](value: T, next: Stack[T]) extends Stack[T] {
    override def push[A >: T](element: A): Stack[A] = NonEmptyStack[A](element, this)

    override def pop[A >: T]: Stack[A] = next

    override def peek[A >: T](): Stack[A] = {
      println(value)
      this
    }
  }

  case class EmptyStack[+T]() extends Stack[T] {
    override def push[A >: T](element: A): Stack[A] = NonEmptyStack[A](element, this)

    override def pop[A >: T]: Stack[A] = throw new NoSuchElementException("Pop on Empty Stack")

    override def peek[A >: T](): Stack[A] = throw new NoSuchElementException("Peek on Empty Stack")
  }

}

object CustomStack {
  def main(args: Array[String]): Unit = {
    val emptyStack = EmptyStack[Int]()
    val output = emptyStack.push(5).push(6).push(7).pop.pop.peek()

    println(output.toString)

  }

}
