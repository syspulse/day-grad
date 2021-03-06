package io.syspulse.daygrad

import java.time._
import java.time.format._
import java.time.temporal._
import java.util.Locale
import scala.math.abs
import scala.util.Random


abstract class Colorful(r:Int,g:Int,b:Int) {
  def get = s"${r},${g},${b}"
}

case class Color(r:Int,g:Int,b:Int) extends Colorful(r,g,b)

case class RandomColor() extends Colorful(abs(Random.nextInt % 256),abs(Random.nextInt % 256),abs(Random.nextInt % 256))


trait Gradient {
  type T
  val empty: Colorful
  val full: Colorful
  val range: Seq[Colorful]
  def getColor(data:T):Colorful
}

trait GradientGithub extends Gradient {
  val empty = Color(235, 237, 240)
  val full = Color(33, 110, 57)
  val range = IndexedSeq(
    Color(155,233,168),
    Color(64, 196, 99),
    Color(48, 161, 78),
    full
  )
}

abstract class GradientRange(min:Int,max:Int) extends Gradient {
  type T = Int
  override def getColor(data:Int) = if(data < min) empty else if(data>=max) full else range(data / ((max-min+1)/range.size + 1))
}

class GradientRandom extends Gradient {
  type T = Int
  val random = RandomColor()
  val empty = random
  val full = random
  val range: Seq[Colorful] = Seq(random)
  override def getColor(data:Int) = random
}

class GradientBlack extends Gradient {
  type T = Any
  val black = Color(0,0,0)
  val empty = black
  val full = black
  val range: Seq[Colorful] = Seq(black)
  override def getColor(data:Any) = black
}

object Render {

  def renderHTML[T](grid: Grid[T], brush: Day[T] => Colorful, tip: Day[T] => String): String = {

    var mOffset = 0
    val gridHTML = grid.months.zipWithIndex.map{ case(m,mIndex) => {
        
        val weeksHTML = m.weeks.zipWithIndex.map{ case(w,wIndex) => {
        
          val dayHTML = 
          w.days.zipWithIndex.map{ case(d,dIndex) => { 
            val color = brush(d).get
            val tipTitle = tip(d)

            val title = s"${d.day._1}-${m.month._2} (${d.day._2}): ${tipTitle}"
            
            s"""         <rect x="12" y="${dIndex*15}" width="11" height="11" class="day" style="fill:rgb(${color})"><title>${title}</title></rect>\n"""

          }}.mkString

          s"""       <g transform="translate(${mOffset + wIndex*16},0)">\n${dayHTML}       </g>\n"""
        
        }}.mkString
      
        val monthHTML = s"""       <text x="${12 + mOffset }" y="-8" class="month">${m.month._2}</text>\n"""

        mOffset = mOffset + m.weeks.size * 16
        
        weeksHTML + monthHTML

      }

    
    }.mkString

    val weekdaysHTML = grid.weekDays.zipWithIndex.map{ case(wd,i) => {
      s"""<text text-anchor="start" class="weekday" dx="-10" dy="${8 + 15 * i}">${wd}</text>"""
    }}

    val indexHTML = 
      s"""
<!DOCTYPE html>
<html>
<head>
    <link rel="stylesheet" href="daygrad.css">
</head>
<body>

<svg width="928" height="128">
    <g transform="translate(10,20)">
    ${gridHTML}
    ${weekdaysHTML}
    </g>
</svg>
</body>
</html>
      """

    indexHTML

  }
}


