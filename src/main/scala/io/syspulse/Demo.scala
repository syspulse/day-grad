package io.syspulse

import java.time._
import java.time.format._

import java.time.temporal._
import java.util.Locale

import scala.math.abs
import scala.util.Random
import scala.io.StdIn

object Demo extends App {
  val past = if(args.size < 2) 12 else args(1).toInt 
  val max = if(args.size < 3) 5 else args(2).toInt
  val min = if(args.size < 4) 1 else args(3).toInt 
  val zone = "America/Los_Angeles"
  val locale = "en_US"

  val defaultDayGrid = DayGrid(tz = ZoneId.of(zone), locale = new Locale(locale))
  val defaultGrid = defaultDayGrid.getGrid(past = past)

  Console.err.println(s"Locale: ${locale}\nTZ: ${zone}\npast=${past} month\nrange=(${min}..${max})")

  val (grid,brush,tip) = 
    (if(args.size==0) "rand" else args(0)) match {
      
      case "rand" => (defaultGrid,(d:Day[Int]) => RandomColor(),(d:Day[Int])=>"")
      
      case "github" => (defaultGrid,(d:Day[Int]) => { 
          (new GradientRange(min,max) with GradientGithub).getColor(Random.between(min-1,max+1))},
          (d:Day[Int])=>""
        )
      
      case "git" => {
        val in = Iterator.continually(StdIn.readLine).takeWhile(_ != null)

        val gradient = (new GradientRange(min,max) with GradientGithub)
        (
          DayGridHits(tz = ZoneId.of(zone), locale = new Locale(locale)).getGridHits(past=past).mapTimeHitsGithub(in.toSeq),

          (d:Day[Int]) => {
            val hits = d.data.getOrElse(0)
            gradient.getColor(hits)
          },

          (d:Day[Int]) => s"${d.data.getOrElse(0)} change(s)"
        )
      }

      case _ => (defaultGrid,(d:Day[_]) => Color(0,0,0), (d:Day[_])=>"")
    }

  val html = DataGridRender.renderHTML(grid, brush, tip)
  println(html)

  // val indexFile = os.pwd / "index.html"
  // os.write.over(indexFile,html)
}