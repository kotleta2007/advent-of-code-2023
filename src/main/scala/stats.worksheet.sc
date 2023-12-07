import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

val url = "https://adventofcode.com/2023/stats"
val browser = JsoupBrowser()
val doc = browser.get(url)

val current = 7
val solved = 4

val stats = doc >> text("main")
val currentDays = 
    stats.split("\n").toList
    .takeRight(current)
    .map(_.split(" +").toList.drop(1).dropRight(1).map(_.toInt)).reverse
val solvedDays = currentDays.take(solved)

val solvedCount = solvedDays.map(d => d.drop(1).sum).sum
val currentCount = currentDays.map(d => d.drop(1).sum).sum

val topPercent = (1 - solvedCount.toFloat / currentCount.toFloat) * 100
println("You are in the top " + math.ceil(topPercent).toInt + "%!")
