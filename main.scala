//> using dep org.jsoup:jsoup:1.18.1
//> using dep net.ruippeixotog::scala-scraper:3.1.1

import org.jsoup.*
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.ArrayBuffer

sealed abstract class StoreChecker {
  def result: String
}

case class SteamChecker() extends StoreChecker {
  val html = Jsoup.connect("https://store.steampowered.com/search/?sort_by=Price_ASC&supportedlang=russian%2Cenglish&specials=1&ndl=1").get()
  val gamesList = html.select("a.search_result_row").asScala
  val freeGames = 
    for g <- gamesList if g.select("div.discount_pct").text == "-100%"
    yield (g.attr("innerText"), g.attr("href"))
  override def result = 
    freeGames match {
      case ArrayBuffer() => "No free games on Steam!"
      case buf => "There are free games!"
    }
}

@main def main() = {
  val steam = new SteamChecker
  val checkers = (steam)
  checkers.foreach(_.println(result))
}