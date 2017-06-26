package forcomp


object Anagrams {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]

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

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = ???

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = ???
}
