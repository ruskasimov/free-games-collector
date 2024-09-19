//> using dep org.jsoup:jsoup:1.18.1
//> using dep net.ruippeixotog::scala-scraper:3.1.1
//> using toolkit default

import org.jsoup.*
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.ArrayBuffer

abstract sealed class StoreChecker(val link: String) {
  def result: String
}

case object SteamChecker extends StoreChecker("https://store.steampowered.com/search/?sort_by=Price_ASC&supportedlang=russian%2Cenglish&specials=1&ndl=1") {
  val html = Jsoup.connect(link).get()
  val gamesList = html.select("a.search_result_row").asScala
  val freeGames = 
    for g <- gamesList if g.select("div.discount_pct").text == "-100%"
    yield (g.attr("innerText"), g.attr("href"))
  override def result = 
    freeGames match {
      case ArrayBuffer() => "No free games on Steam!\n"
      case buf => "Free games on Steam:\n" + 
        {for (n, l) <- buf yield s"${n}: ${l}"}.mkString("\n") + "\n"
    }
}

case object EpicChecker extends StoreChecker("https://store-site-backend-static.ak.epicgames.com/freeGamesPromotions") {
  val html = Jsoup.connect(link).ignoreContentType(true).execute().body()
  val json = ujson.read(html)
  val freeGames = json("data")("Catalog")("searchStore")("elements")
    .arr.filter(_("price")("totalPrice")("discountPrice").num == 0).map(elem => elem("title"))
  override def result = "Free games on Epic Games Store:\n" + freeGames.mkString("\n")
}

@main def main() = {
  val steam = SteamChecker
  val epic = EpicChecker
  val checkers = List(steam, epic)
  //println(steam.result)
  checkers.map(checker => checker.result).foreach(println(_))
}