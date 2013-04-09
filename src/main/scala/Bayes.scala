package bayes

import collection.mutable

trait Classifier {

  type Category = String
  type Word = String

  /**
   * TODO we could move this to a durable store like mySQL
   *
   * A mutable map storing [
   */
  val wordCount = mutable.Map[(Category, Word), Int]()

  val categoryCount = mutable.Map[String, Int]()

  def incrementCategoryCount(category: Category) = {
    val current = categoryCount.getOrElse(category, 0)
    categoryCount.put(category, 1+current)
  }

  def incrementWordCount(category: Category, word: Word): Unit = {
    val current = wordCount.get((category, word)).getOrElse(0)
    wordCount.put((category, word), 1 + current)
  }

  /**
   * The number of times a feature occurs in a category
   *
   */
  def featureCount(feature: Word, category: Category): Double = {
    wordCount.get((category,feature)) match {
      case Some(v: Int) => v.toDouble
      case None => 0.0
    }
  }

  /**
   * The number of times a category occurs
   *
   */
  def catCount(category: Category): Double = {
    categoryCount.get(category) match {
      case Some(v: Int) => v.toDouble
      case None => 0.0
    }
  }
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/*
 * Expand the classifier to handle bigrams
 */
trait BiGramClassifier {

}

object Bayes extends Classifier {

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** General Probability */

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
    weightedAverage(1.0, 0.5, totals, basicProbability)
  }

  /**
   * What is the probability that a feature belongs in a given category?
   *
   */
  def probability(text: String, category: String) =
    documentProbability(text, category) * categoryProbability(category)

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  def trainBiGrams(category: String, text: String): Unit = {
    val bigrams = new Tokenizer(text).biGrams()
    //bigrams.foreach { word => incrementWordCount(category, word) }
  }

  def train(category: String, text: String): Unit = {
    val tokens = new Tokenizer(text).tokenizeFiltered()
    tokens.foreach { word => incrementWordCount(category, word) }
    incrementCategoryCount(category)
  }

  def classifyCategories(text: String): List[Pair[String, Double]] = {
    val results = categoryCount.map { category =>
      (category._1, documentProbability(text, category._1))
    }
    results.toList
  }

  /** Which category does the text belong to? */
  def classify(text: String): Option[String] = {
    classifyCategories(text) match {
      case v@x::xs => v.maxBy(_._2) match {
        case Pair(c,_) => Some(c)
      }
      case _ => None // If we match here there are no categories
    }
  }
}
