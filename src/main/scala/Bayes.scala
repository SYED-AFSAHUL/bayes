package bayes

import collection.mutable
import scala.io.Source.{fromFile}

class Tokenizer(val text: String) {

  type wordList = List[String]

  val normalize = (text: String) => text.toLowerCase

  val words = (text: String) => text.split(" ")

  private def readStopWords() = fromFile("stopWords").getLines.toList

  def filterStopWords(words: wordList): wordList = {
    val stopWords = readStopWords()
    words.filter { w => !stopWords.contains(w) }
  }

  def tokenize(): wordList =
    (words compose normalize)(text).toList

  def tokenizeFiltered(): wordList =
    filterStopWords(tokenize)

  def ngramTokenizer(n: Int, text: String) = sys.error("TODO")
}

trait Classifier {

  val wordCount = mutable.Map[(String, String), Int]()

  val categoryCount = mutable.Map[String, Int]()

  def incrementCategoryCount(category: String) = {
    val current = categoryCount.getOrElse(category, 0)
    categoryCount.put(category, 1+current)
  }

  def incrementWordCount(category: String, word: String): Unit = {
    val current = wordCount.get((category, word)).getOrElse(0)
    wordCount.put((category, word), 1 + current)
  }

  def wordOccurs(category: String, word: String): Int = {
    wordCount.get((category,word)) match {
      case Some(v: Int) => v
      case None => 0
    }
  }
}

object Bayes extends Classifier {

  def wordMetrics(word: String, category: String): Pair[Int, Int] =
    ( wordOccurs(category, word), categoryCount(category) )

  def singleWordProbability(word: String, category: String)= {
    wordMetrics(word, category)
  }

  /**
   * Train our classifier
   *
   */
  def train(category: String, text: String): Unit = {
    val tokens = new Tokenizer(text).tokenizeFiltered()
    tokens.foreach { word => incrementWordCount(category, word) }
    incrementCategoryCount(category)
  }

  def classify(text: String) = {
    val tokenizer = new Tokenizer(text)
    tokenizer.tokenize
  }
}
