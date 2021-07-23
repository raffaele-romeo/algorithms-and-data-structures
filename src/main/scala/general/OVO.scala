package general

/*
Imagine the list was circular. So in the above example, apple would follow orange:
take(["apple", "pear", "lemon", "orange"], 5)
# ["apple", "pear", "lemon", "orange", "apple"]
*/

object OVO {

  def take(input: List[String], n: Int): List[String] = {
    LazyList.continually(input).flatten.take(n).toList
  }

  def main(args: Array[String]): Unit = {
    val input = List("apple", "pear", "lemon", "orange")

    println(take(input, 150).mkString(", "))
  }


}
