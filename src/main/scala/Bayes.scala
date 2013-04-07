package bayes

import collection.mutable

import scala.io.Source.{fromFile}

trait NGramTokenizer {

  def nGrams(n: Int, text: String) = sys.error("TODO")

  def biGrams(text: String) = sys.error("TODO")
}

class Tokenizer(val text: String) {

  type wordList = List[String]

  val normalize = (text: String) => text.toLowerCase
  val words = (text: String) => text.split(" ")

  private def readStopWords() = fromFile("stopWords").getLines.toList

  private val badChars = List(',', ';', '-', '?', '\n', '!') map (_.toString)

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

  /**
   * The number of times a feature occurs in a category
   *
   */
  def featureCount(feature: String, category: String): Double = {
    wordCount.get((category,feature)) match {
      case Some(v: Int) => v.toDouble
      case None => 0.0
    }
  }

  /**
   * The number of times a category occurs
   *
   */
  def catCount(category: String): Double = {
    categoryCount.get(category) match {
      case Some(v: Int) => v.toDouble
      case None => 0.0
    }
  }
}

object Bayes extends Classifier {

  def wordMetrics(word: String, category: String): Pair[Double, Double] =
    ( featureCount(word, category), catCount(category) )

  def categoryProbability(category: String): Double =
    catCount(category) / categoryCount.values.sum

  def documentProbability(text: String, category: String) = {
    val tokens = new Tokenizer(text).tokenizeFiltered()
    var p = 1.0
    tokens.foreach { term =>
      p *= weightedProbability(term, category)
    }
    p
  }

  /**
   * The probability that an item is in a category
   * P(C|D) => featureCount / categoryCount
   */
  def featureProbability(feature: String, category: String): Double = {
    val metrics = wordMetrics(feature, category)
    val featureCount = metrics._1
    val categoryCount = metrics._2
    if (categoryCount == 0) 0.0
    else featureCount / categoryCount
  }

  /** The number of times a feature appears across ALL categories */
  def featureTotals(feature: String): Int = {
    val t = categoryCount flatMap {
      case (k,_) => {
        if(wordCount.contains((k, feature))) Some(wordCount((k, feature)))
        else None
      }}
    t.sum
  }

  /** Calculate the weighted average probability */
  def weightedAverage(weight: Double, assumed: Double, totals: Double, basicProbability: Double) = {
    val x = (weight * assumed + totals * basicProbability)
    val y = weight + totals
    x / y
  }

  /**
   * The balanced weighted probability for a feature appearing in a category
   *
   */
  def weightedProbability(feature: String, category: String) = {
    val basicProbability = featureProbability(feature, category)
    val totals = featureTotals(feature)
    println(basicProbability)
    weightedAverage(1.0, 0.5, totals, basicProbability)
  }

  /**
   * What is the probability that a feature belongs in a given category?
   *
   */
  def probability(text: String, category: String) =
    documentProbability(text, category) * categoryProbability(category)

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
