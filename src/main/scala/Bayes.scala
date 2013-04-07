package bayes

object Tokenizer {

  def normalize(text: String): String = text.toLowerCase

  def words(text: String): List[String] = words.split(" ")

  def tokenize(text: String): List[String] = normalize andThen words

}
