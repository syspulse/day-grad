package io.syspulse

import java.time._
import java.time.format._
import java.time.temporal._
import java.util.Locale


case class Color(r:Int,g:Int,b:Int) {
  def get = s"${r},${g},${b}"
}

case class RandomColor() {
  def R = scala.math.abs(scala.util.Random.nextInt % 256)
  def get = Color(R,R,R).get
}

trait Gradient {
  type T
  def getColor(data:T):Color
}

trait GithubGradient extends Gradient {
  val empty = Color(235, 237, 240)
  val range = IndexedSeq(
    Color(33, 110, 57),
    Color(48, 161, 78),
    Color(64, 196, 99),
    Color(155,233,168),
    empty
  )
}

class GithubIntGradient[T](min:T,max:T) extends GithubGradient {
  type T = Int
  override def getColor(data:Int) = if(data < 0) empty else range(data % range.size)
}

object DataGridRender {

  def renderHTML(grid: Grid): String = {

    var mOffset = 0
    val gridHTML = grid.months.zipWithIndex.map{ case(m,mIndex) => {
        
        val weeksHTML = m.weeks.zipWithIndex.map{ case(w,wIndex) => {
        
          val dayHTML = 
          w.days.zipWithIndex.map{ case(d,dIndex) => { 
            val color = RandomColor().get
            val tip = s"${d.day._1}-${m.month._2} (${d.day._2})"
            
            s"""         <rect x="12" y="${dIndex*15}" width="11" height="11" class="day" style="fill:rgb(${color})"><title>${tip}</title></rect>\n"""

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
    <link rel="stylesheet" href="daygrid.css">
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


