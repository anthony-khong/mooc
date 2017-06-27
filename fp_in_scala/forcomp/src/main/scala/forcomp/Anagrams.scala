package forcomp


object Anagrams {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  type Dictionary = Map[Occurrences, List[Word]]

  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(word: Word): Occurrences = {
    word.toLowerCase.groupBy(identity).mapValues(_.size).toList.sorted
  }

  def sentenceOccurrences(sentence: Sentence): Occurrences = {
    wordOccurrences(sentence.mkString)
  }

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary groupBy wordOccurrences
  }

  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences(wordOccurrences(word))
  }

  // First attempt: probably inefficient
  // def combinations(occs: Occurrences): List[Occurrences] = {
  //   val word = occs map {case (ltr, n) => ltr.toString * n} mkString
  //   val allWords = (0 to word.length) flatMap (x => word combinations x)
  //   (allWords map wordOccurrences).toList
  // }

  def combinations(occs: Occurrences): List[Occurrences] = occs match {
    case Nil => List(List())
    case (ltr, count) :: tail => {
      val tailCombos = combinations(tail)
      val combosWithHead = for {
        n <- 1 to count
        combo <- tailCombos
      } yield (ltr, n) :: combo
      combosWithHead.toList ++ tailCombos
    }
  }

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def substractOcc(map: Map[Char, Int], occ: (Char, Int)): Map[Char, Int] = {
      val (ltr, n) = occ
      val updatedCount = map(ltr) - n
      if (updatedCount == 0) map - ltr else map + (ltr -> updatedCount)
    }
    val occMap = x.toMap withDefaultValue 0
    val substractedMap = (y foldLeft occMap)(substractOcc)
    substractedMap.toList.sorted
    }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val sentenceOccs = sentenceOccurrences(sentence)
    val dict = dictionaryByOccurrences withDefaultValue List()
    occAnagrams(sentenceOccs, dict)
  }

  def occAnagrams(sentenceOccs: Occurrences, dict: Dictionary): List[Sentence] = {
    if (sentenceOccs.isEmpty) List(List())
    else for {
      wordOccs <- combinations(sentenceOccs)
      word <- dict(wordOccs)
      otherWords <- occAnagrams(subtract(sentenceOccs, wordOccs), dict)
    } yield word :: otherWords
  }
}
