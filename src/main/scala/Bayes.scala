package bayes

class Tokenizer(val text: String) {

  val normalize = (text: String) => text.toLowerCase

  val words = (text: String) => text.split(" ")

  def tokenize(): List[String] =
    (words compose normalize)(text).toList
}

trait Classifier {
  def classify(text: String)
}

object Bayes extends Classifier {

  // Map(category -> Map(word -> count))
  var wordCount = scala.collection.mutable.Map[String, Map[String, Int]]()

  var categoryCount = collection.mutable.Map[String, Int]()

  def incrementCategoryCount(category: String) = {
    val count = categoryCount.getOrElseUpdate(category, 0)
    categoryCount = categoryCount.map {
      case(k, v) => {
        if (k == category) (k, v + 1)
        else (k,v)
      }
    }
  }

  def incrementWordCount(category: String, word: String) =
    sys.error("todo")

  def classify(text: String) = {
    val tokenizer = new Tokenizer(text)
    tokenizer.tokenize
  }
}
