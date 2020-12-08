package io.syspulse

import java.time._
import java.time.format._
import java.time.temporal._
import java.util.Locale

class DayGrid(tz:ZoneId = ZoneId.systemDefault, locale:Locale=Locale.getDefault()) {
  case class Pos(x:Int,y:Int)
  case class Month(month:(Int,String),duration:Int,pos:Pos,weeks:Seq[Week]=Seq())
  case class Week(days:Seq[Day])
  case class Day(day:(Int,String),pos:Pos)

  case class Grid(months:Seq[Month]) {
    override def toString = {
      months.map( m => {
        s"month=${m.month},${m.duration}: \n" + 
        m.weeks.map( w=> {
          s"week=" + 
          w.days.map( d=> { 
            s"${d}, "
          }).mkString + "\n" 
        }).mkString +"\n"
      }).mkString
    }
  }

  val weekDuration = 7
  
  val weekDayStart = WeekFields.of(locale).getFirstDayOfWeek()

  def getGrid(startTime:Option[LocalDateTime]=None, past:Int=12,boxSize:Int=10) = generate(startTime,past,boxSize)

  def generate(startTime:Option[LocalDateTime], past:Int,boxSize:Int):Grid = {
    
    val t0 = startTime.getOrElse(LocalDateTime.now(tz))
    val currentMonth = t0.getMonthValue

    val mm = for(i <- 0 to past) yield {
      val m = t0.minusMonths(i)
      val mName = m.getMonth()

      val start = m.`with`(TemporalAdjusters.previous(weekDayStart));
      //DayOfWeek.of(((firstDayOfWeek.getValue() + 5) % DayOfWeek.values().length) + 1)
      val end = {
        val nexMonth = m.plusMonths(1)
        if(nexMonth.isAfter(t0))
          t0.plusDays(1) // because of decrement
        else
          nexMonth.`with`(TemporalAdjusters.previous(weekDayStart));
      }

      val duration = Duration.between(start, end).toDays().toInt
      val days = for(d <- 0 to duration - 1) yield {
        val day = start.plusDays(d).getDayOfMonth
        val dayStr = start.plusDays(d).getDayOfWeek.toString
        Day(day=(day,dayStr),Pos(0,0))
      }

      val week = days.grouped(weekDuration).map( dd => Week(dd) ).toSeq

      Month(month=(m.getMonthValue,mName.toString),duration,Pos(0,0),week)
    }

    Grid(mm.reverse)
  }

}
