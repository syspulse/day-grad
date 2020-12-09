package io.syspulse.daygrad

import java.time._
import java.time.format._
import java.time.temporal._
import java.util.Locale

case class Month[T](month:(Int,String),duration:Int,weeks:Seq[Week[T]]=Seq())
case class Week[T](days:Seq[Day[T]])
case class Day[+T](day:(Int,String),date:LocalDate,data:Option[T] = None)

class Grid[T](val months:Seq[Month[T]],val weekDays:Seq[String]) {
    
    def map(f:(Day[T])=>Day[T]):Grid[T] = {
      new Grid[T](
        months.map( m => {
          Month[T](month = m.month, duration = m.duration, weeks = m.weeks.map( w => Week(w.days.map( d => f(d)))))
        }),
        weekDays = this.weekDays
      )
    }
    
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

class DayGrad(val tz:ZoneId, val locale:Locale) {
  
  val weekDuration = 7
  
  val weekDayStart = WeekFields.of(locale).getFirstDayOfWeek

  def getWeekDays:Seq[String] = for(d <- 0 to weekDuration - 1) yield prettyDay(WeekFields.of(locale).getFirstDayOfWeek().plus(d).toString())

  def prettyMonth(m:String) = if(m.size<3) m else m.substring(0,3).toLowerCase.capitalize
  def prettyDay(d:String) = prettyMonth(d)

  def getGrid[T](startTime:Option[LocalDateTime]=None, past:Int=12,boxSize:Int=10):Grid[T] = {
    val (mm,weekDays) = generate[T](startTime,past,boxSize)
    new Grid[T](mm, weekDays)
  }

  protected def generate[T](startTime:Option[LocalDateTime], past:Int,boxSize:Int):(Seq[Month[T]],Seq[String]) = {
    
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
        val dayStr = prettyDay(start.plusDays(d).getDayOfWeek.toString)
        Day(day=(day,dayStr),date=start.plusDays(d).toLocalDate())
      }

      val week = days.grouped(weekDuration).map( dd => Week[T](dd) ).toSeq

      Month[T](month=(m.getMonthValue,prettyMonth(mName.toString)),duration,week)
    }

    (mm.reverse, getWeekDays)
  }
}


object DayGrad {
  def apply(tz:ZoneId = ZoneId.systemDefault, locale:Locale=Locale.getDefault()) = new DayGrad(tz,locale)
}
