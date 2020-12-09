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

  val d = new DayGrid(tz = ZoneId.of("America/Los_Angeles"), locale = new Locale("en_US"))
  val g = d.getGrid(past = past)

  Console.err.println(s"Locale: ${d.locale}\nTZ: ${d.tz}\npast=${past} month\nrange=(${min}..${max})")

  val (grid,brush,tip) = 
    (if(args.size==0) "rand" else args(0)) match {
      
      case "rand" => (g,(d:Day) => RandomColor(),(d:Day)=>"")
      
      case "github" => (g,(d:Day) => { 
          (new GradientRange(min,max) with GradientGithub).getColor(Random.between(min-1,max+1))},
          (d:Day)=>""
        )
      
      case "git" => {
        val in = Iterator.continually(StdIn.readLine).takeWhile(_ != null)

        val gradient = (new GradientRange(min,max) with GradientGithub)
        (
          g.mapTimeHitsGithub(in.toSeq),

          (d:Day) => {
            val data = d.data.asInstanceOf[Option[Long]].getOrElse(0)
            gradient.getColor(data.asInstanceOf[Int])
          },

          (d:Day) => s"${d.data.asInstanceOf[Option[Long]].getOrElse(0)} change(s)"
        )
      }

      case _ => (g,(d:Day) => Color(0,0,0), (d:Day)=>"")
    }

  val html = DataGridRender.renderHTML(grid, brush, tip)
  println(html)

  // val indexFile = os.pwd / "index.html"
  // os.write.over(indexFile,html)
}