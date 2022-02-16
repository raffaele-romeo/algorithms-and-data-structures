package general

final case class WordWithFrequencyCount(word: String, characterFrequencyCount: Map[Char, Int])
final case class WordWithDistinctCharacterCount(word: String, distinctCharacterCount: Int)

object CoremontTask {

  def buildFrequencyCount(word: String): WordWithFrequencyCount = {
    val wordFrequencyCount = word.foldLeft(Map[Char, Int]().empty) {
      case (map, next) => map + ((next, map.getOrElse(next, 0) + 1))
    }
    WordWithFrequencyCount(word, wordFrequencyCount)
  }

  def groupWordsByFrequencyCount(wordsWithFrequencyCount: List[WordWithFrequencyCount]): List[(String, Int)] =
    wordsWithFrequencyCount.groupBy(_.characterFrequencyCount).map {
      case (_, list) => (list.map(_.word).mkString(", "), list.size)
    }.toList

  def selectLargestSetOfWordsThatAreAnagramsOfEachOther(groupedWordsByFrequencyCount: List[(String, Int)]): String =
    groupedWordsByFrequencyCount
    .maxBy(_._2)
    ._1

  def distinctCharacters(word: String): WordWithDistinctCharacterCount =
    WordWithDistinctCharacterCount(word, word.toList.distinct.size)

  def wordWithMaxDistinctCharacter(wordsWithDistinctCharacterCount: List[WordWithDistinctCharacterCount]): String =
    wordsWithDistinctCharacterCount
      .maxBy(_.distinctCharacterCount)
      .word

  def countOfWordsGroupedByFirstChar(words: List[String]): List[(Char, Int)] =
    words.groupBy(_.toList.head.toUpper).view.mapValues(_.size).toList
      .sortBy(_._1)


  def main(args: Array[String]): Unit = {
    //Handle errors
    val inputFile = scala.io.Source.fromResource("words.txt").getLines().toList

    val wordsWithFrequencyCount = inputFile.map(buildFrequencyCount)
    val groupedWordByFrequencyCount = groupWordsByFrequencyCount(wordsWithFrequencyCount)

    println(selectLargestSetOfWordsThatAreAnagramsOfEachOther(groupedWordByFrequencyCount))

    val wordsDistinctCharacterCount = inputFile.map(distinctCharacters)
    println(wordWithMaxDistinctCharacter(wordsDistinctCharacterCount))

    println(countOfWordsGroupedByFirstChar(inputFile).mkString(", "))
  }
}
