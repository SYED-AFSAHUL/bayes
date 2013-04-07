package bayes

import collection.mutable

class Tokenizer(val text: String) {

  val normalize = (text: String) => text.toLowerCase

  val words = (text: String) => text.split(" ")

  def tokenize(): List[String] =
    (words compose normalize)(text).toList

  def ngramTokenizer(n: Int, text: String) = sys.error("TODO")
}

trait Classifier {
  def classify(text: String)
}

case class Word(category: String, word: String, count: Int)

object Bayes extends Classifier {

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

  def wordAppearsInCategory(category: String, word: String): Int = {
    wordCount.get((category,word)) match {
      case Some(v: Int) => v
      case None => 0
    }
  }

  def classify(text: String) = {
    val tokenizer = new Tokenizer(text)
    tokenizer.tokenize
  }
}
