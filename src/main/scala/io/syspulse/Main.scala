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

  val d = new DayGrid(tz = ZoneId.of("America/Los_Angeles"), locale = new Locale("en_US"))
  val g = d.getGrid(past = past)

  Console.err.println(s"Locale: ${d.locale}\nTZ: ${d.tz}\npast=${past} month")

  val (grid,brush) = 
    (if(args.size==0) "rand" else args(0)) match {
      
      case "rand" => (g,(d:Day) => RandomColor())
      
      case "github" => (g,(d:Day) => { 
          (new GradientRange(1,5) with GradientGithub).getColor(Random.between(0,5))}
        )
      
      case "git" => {
        // git log --date=raw
        val in = Iterator.continually(StdIn.readLine).takeWhile(_ != null)

        //val tss = in.map( s => (s.split("\\s+")(1).toLong) ).toSeq

        (g.mapTimeHitsGithub(in.toSeq),(d:Day) => {
          val data = d.data.asInstanceOf[Option[Long]].getOrElse(0)
          (new GradientRange(1,5) with GradientGithub).getColor(data.asInstanceOf[Int])
        })
      }

      case _ => (g,(d:Day) => Color(0,0,0))
    }

  val html = DataGridRender.renderHTML(grid,brush)
  println(html)

  val indexFile = os.pwd / "index.html"
  os.write.over(indexFile,html)
}