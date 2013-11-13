package bayes

import scala.io.Source.{fromFile}

trait NGramTokenizer {
  def nGrams(n: Int): List[List[String]]
}

class Tokenizer(val text: String) extends NGramTokenizer {

  type wordList = List[String]

  val normalize = (text: String) => text.toLowerCase

  val words = (text: String) => text.split(" ")

  private def readStopWords() = fromFile("stopWords").getLines.toList

  private val badChars = List(',', ';', '-', '?', '\n', '!') map (_.toString)

  /** Split the text into ngrams */
  def nGrams(n: Int) = tokenize().sliding(n).toList
  
  def biGrams() = nGrams(2)

  def stripPunctuation(text: String): String = {
    def replaceAll(chars: List[String], t: String): String = chars match {
      case x::xs => replaceAll(xs, t.replace(x,""))
      case Nil => t
    }
    replaceAll(badChars, text)
  }

  def filterStopWords(words: wordList): wordList = {
    val stopWords = readStopWords()
    words.filter { w => !stopWords.contains(w) }
  }

  def tokenize(): wordList =
    (words compose normalize)(text).toList

  def tokenizeFiltered(): wordList =
    filterStopWords(tokenize)
}

