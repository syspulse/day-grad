package io.syspulse.daygrad

import java.time._
import java.time.format._
import java.time.temporal._
import java.util.Locale

import io.syspulse.daygrad.Month

class GridHits(months:Seq[Month[Int]],weekDays:Seq[String]) extends Grid[Int](months,weekDays){

  // expected git log --date=raw
  // "Date:   1607503394 +0200"
  def mapTimeHitsGithub(timeSeriesGit:Seq[String]):Grid[Int] = {
    mapTimeHits( timeSeriesGit.map( s => (s.split("\\s+")(1).toLong)))
  }

  def mapTimeHits(timeSeries:Seq[Long]):Grid[Int] = {
    // create a map of timeseries by truncating to Day and grouping
    val tssMap = timeSeries.map( ts => {
      LocalDate.ofInstant(Instant.ofEpochSecond(ts), ZoneId.systemDefault())
    }).groupBy(v=>v).map{ case(k,v) => k -> v.size }
    
    map( d => {
      val hits = tssMap.getOrElse(d.date,0)
      d.copy(data = Some(hits))
    })
  }        
}


class DayGradHits(tz:ZoneId, locale:Locale) extends DayGrad(tz,locale) {
     
    def getGridHits(startTime:Option[LocalDateTime]=None, past:Int=12,boxSize:Int=10):GridHits = {
      val (mm,weekDays) = generate[Int](startTime,past,boxSize)
      new GridHits(mm, weekDays)
    }
}

object DayGradHits {
  def apply(tz:ZoneId = ZoneId.systemDefault, locale:Locale=Locale.getDefault()) = new DayGradHits(tz,locale)
}
