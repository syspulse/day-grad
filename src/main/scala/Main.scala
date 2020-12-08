import java.time._
import java.time.format._

import java.time.temporal._
import java.util.Locale

import io.syspulse.DayGrid

object Main extends App {
  
  val d = new DayGrid(tz = ZoneId.of("America/Los_Angeles"), locale = new Locale("en_US"))

  println(s"${d.getGrid()}")
}