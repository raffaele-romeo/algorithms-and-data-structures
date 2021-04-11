package maps

object FrequencyQuery {
  def freqQuery(queries: Array[Array[Int]]): Array[Int] = {
    val value = queries.foldLeft((Map[Int, Int](), Map[Int, Int](), Vector[Int]())) {
      case ((count, frequencies, output), elem) =>
        elem.head match {
          case 1 =>
            val newCount = count.+(elem(1) -> (count.getOrElse(elem(1), 0) + 1))

            val updateOldElement = count.getOrElse(elem(1), 0) -> (frequencies.getOrElse(count.getOrElse(elem(1), 0), 0) - 1)
            val updateNewElement = newCount.getOrElse(elem(1), 0) -> (frequencies.getOrElse(newCount.getOrElse(elem(1), 0), 0) + 1)
            val newFrequencies = frequencies.+(updateOldElement, updateNewElement)

            (newCount, newFrequencies, output)
          case 2 =>
            if (count.getOrElse(elem(1), 0) > 0) {
              val newCount = count.+(elem(1) -> (count.getOrElse(elem(1), 0) - 1))

              val updateOldElement = count.getOrElse(elem(1), 0) -> (frequencies.getOrElse(count.getOrElse(elem(1), 0), 0) - 1)
              val updateNewElement = newCount.getOrElse(elem(1), 0) -> (frequencies.getOrElse(newCount.getOrElse(elem(1), 0), 0) + 1)
              val newFrequencies = frequencies.+(updateOldElement, updateNewElement)

              (newCount, newFrequencies, output)
            } else
              (count, frequencies, output)
          case 3 =>
            if (frequencies.getOrElse(elem(1), 0) > 0)
              (count, frequencies, output.:+(1))
            else
              (count, frequencies, output.:+(0))
        }
    }
    value._3.toArray
  }

  def main(args: Array[String]): Unit = {
    val input = Array(
      Array(1, 3),
      Array(2, 3),
      Array(3, 2),
      Array(1, 4),
      Array(1, 5),
      Array(1, 5),
      Array(1, 4),
      Array(3, 2),
      Array(2, 4),
      Array(3, 2)
    )

    println(freqQuery(input).mkString(", "))
  }
}
