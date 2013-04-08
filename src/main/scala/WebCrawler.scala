package bayes

import org.jsoup.Jsoup
import org.jsoup.helper.Validate
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

import akka.actor._

// A Scala web crawler for finding training data

object Crawler {

  val wiki = "http://en.wikipedia.org/wiki/Sports"

  def readArticle(url:String): org.jsoup.nodes.Document =
    Jsoup.connect(url).get()

  /**
   * Fetch a node in the document such as the body text
   *
   */
  def fetchNode(url: String, node: String): String = {
    val doc = readArticle("http://en.wikipedia.org/wiki/Sports")
    doc.select(node).text
  }

  def bodyText(url: String): String =
    fetchNode(url, "body")
}

class WebCrawler() extends Actor {

}
