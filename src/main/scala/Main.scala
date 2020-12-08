import java.time._
import java.time.format._

import java.time.temporal._
import java.util.Locale

import io.syspulse._

object Main extends App {
  
  val d = new DayGrid(tz = ZoneId.of("America/Los_Angeles"), locale = new Locale("en_US"))

  val g = d.getGrid()
  
  println(s"${g}")

  val html = DataGridRender.renderHTML(g)
  println(html)

  val indexFile = os.pwd / "index.html"
  os.write.over(indexFile,html)


}