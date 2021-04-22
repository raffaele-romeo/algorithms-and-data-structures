package general

import scala.annotation.tailrec

object OVOInterview {

  object InputData {
    val InputString = "lorem ipsum dolor sit amet consectetur lorem ipsum et mihi quoniam et adipiscing elit.sed quoniam et advesperascit et mihi ad villam revertendum est nunc quidem hactenus ex rebus enim timiditas non ex vocabulis nascitur.nummus in croesi divitiis obscuratur pars est tamen divitiarum.nam quibus rebus efficiuntur voluptates eae non sunt in potestate sapientis.hoc mihi cum tuo fratre convenit.qui ita affectus beatum esse numquam probabis duo reges constructio interrete.de hominibus dici non necesse est.eam si varietatem diceres intellegerem ut etiam non dicente te intellego parvi enim primo ortu sic iacent tamquam omnino sine animo sint.ea possunt paria non esse.quamquam tu hanc copiosiorem etiam soles dicere.de quibus cupio scire quid sentias.universa enim illorum ratione cum tota vestra confligendum puto.ut nemo dubitet eorum omnia officia quo spectare quid sequi quid fugere debeant nunc vero a primo quidem mirabiliter occulta natura est nec perspici nec cognosci potest.videmusne ut pueri ne verberibus quidem a contemplandis rebus perquirendisque deterreantur sunt enim prima elementa naturae quibus auctis virtutis quasi germen efficitur.nam ut sint illa vendibiliora haec uberiora certe sunt.cur deinde metrodori liberos commendas.mihi inquam qui te id ipsum rogavi nam adhuc meo fortasse vitio quid ego quaeram non perspicis.quibus ego vehementer assentior.cur iustitia laudatur mihi enim satis est ipsis non satis.quid est enim aliud esse versutum nobis heracleotes ille dionysius flagitiose descivisse videtur a stoicis propter oculorum dolorem.diodorus eius auditor adiungit ad honestatem vacuitatem doloris.nos quidem virtutes sic natae sumus ut tibi serviremus aliud negotii nihil habemus."

  }

  final case class WordCount(word: String, count: Int)

  final case class Pair(word1: String, word2: String)

  final case class PairWithCount(pair: Pair, count: Int)


  object Question1 {

    def getSentences(input: String): List[String] = input.split("\\.").toList

    def getWords(sentence: String): List[String] = sentence.split(" ").toList

    def groupByWords(input: String): List[WordCount] = getSentences(input).flatMap(getWords)
      .groupBy(identity).view.mapValues(_.length).toList.map {
      case (word, count) => WordCount(word, count)
    }.sortBy(_.count).reverse

    def getPairs(wordsInSentence: List[String]): List[Pair] = {
      @tailrec
      def inner(runningList: List[String], output: List[Pair]): List[Pair] = {
        runningList match {
          case Nil => output
          case _ :: Nil => output
          case head :: second :: Nil => inner(Nil, output :+ Pair(head, second))
          case head :: tail => inner(tail, output :+ Pair(head, tail.head))
        }
      }

      inner(wordsInSentence, List[Pair]())
    }

  }


  def main(args: Array[String]): Unit = {

    import Question1._
    import InputData.InputString

    val wordsOnInputData = getSentences(InputString).flatMap(getWords)

    val wordsCount = wordsOnInputData.size
    val sentencesCount = getSentences(InputString).size
    val lengthLongestWord = wordsOnInputData.map(_.length).max
    val groupedWords = groupByWords(InputString)
    val uniqueWordsCount = groupedWords.count(_.count == 1)


    println(wordsCount)
    println(sentencesCount)
    println(lengthLongestWord)
    println(groupByWords(InputString).mkString(", "))
    println(uniqueWordsCount)
    println(uniqueWordsCount.toDouble / groupedWords.size)
    println(wordsCount.toDouble / sentencesCount)

    val pairs = getSentences(InputString).map(getWords).flatMap(getPairs)
    val output = pairs
      .groupBy(pair => (pair.word1, pair.word2))
      .view
      .mapValues(_.length)
      .toList
      .map {
        case (pair, count) => PairWithCount(Pair(pair._1, pair._2), count)
      }.sortBy(_.count).reverse
      .take(3)

    println(output)

  }

}
