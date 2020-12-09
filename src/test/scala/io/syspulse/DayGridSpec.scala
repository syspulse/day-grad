package io.syspulse

import org.scalatest._
import flatspec._
import matchers._

import java.time._
import java.time.format._
import java.time.temporal._
import java.util.Locale
import scala.util.Random


class DayGridSpec extends AnyFlatSpec with should.Matchers {

  val FMT = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss")

  "Github Daygrid for 8-Dec-2020" should "have last month Decempber and week start on 6-th, Dec" in {
    val d = new DayGrid(tz = ZoneId.of("America/Los_Angeles"), locale = new Locale("en_US"))

    val dd = d.getGrid(
      startTime = Some(LocalDateTime.parse("2020-12-08T00:00:00",FMT))
    )
    
    dd.months.size should be > 0
    dd.months should have size 13

    dd.months.last.weeks.size should be > 0
    dd.months.last.weeks should have size 1

    dd.months.last.weeks(0).days should have size 3
  }

  "Github Daygrid for 8-Dec-2020" should "start weeks on (5-Jan, 2-Feb, 1-Mar, 5-Apr, 3-May, 7-Jun, 5-Jul, 2-Aug, 6-Sep, 4-Oct, 1-Nov, 6-Dec" in {
    val d = new DayGrid(tz = ZoneId.of("America/Los_Angeles"), locale = new Locale("en_US"))

    val dd = d.getGrid(past = 11,
      startTime = Some(LocalDateTime.parse("2020-12-08T00:00:00",FMT))
    )
    
    dd.months.size should be > 0
    dd.months should have size 12


    dd.months(0).month._2 shouldBe "Jan"
    dd.months(0).weeks(0).days(0).day._1 shouldBe 5
    
    dd.months(1).month._2 shouldBe "Feb"
    dd.months(1).weeks(0).days(0).day._1 shouldBe 2

    dd.months(2).month._2 shouldBe "Mar"
    dd.months(2).weeks(0).days(0).day._1 shouldBe 1

    dd.months(3).month._2 shouldBe "Apr"
    dd.months(3).weeks(0).days(0).day._1 shouldBe 5

    dd.months(4).month._2 shouldBe "May"
    dd.months(4).weeks(0).days(0).day._1 shouldBe 3

    dd.months(5).month._2 shouldBe "Jun"
    dd.months(5).weeks(0).days(0).day._1 shouldBe 7

    dd.months(6).month._2 shouldBe "Jul"
    dd.months(6).weeks(0).days(0).day._1 shouldBe 5

    dd.months(7).month._2 shouldBe "Aug"
    dd.months(7).weeks(0).days(0).day._1 shouldBe 2

    dd.months(8).month._2 shouldBe "Sep"
    dd.months(8).weeks(0).days(0).day._1 shouldBe 6

    dd.months(9).month._2 shouldBe "Oct"
    dd.months(9).weeks(0).days(0).day._1 shouldBe 4

    dd.months(10).month._2 shouldBe "Nov"
    dd.months(10).weeks(0).days(0).day._1 shouldBe 1

    dd.months(11).month._2 shouldBe "Dec"
    dd.months(11).weeks(0).days(0).day._1 shouldBe 6
  }

  "Github Daygrid for 8-Dec-2020" should "map to itself" in {
    val d = new DayGrid(tz = ZoneId.of("America/Los_Angeles"), locale = new Locale("en_US"))

    val dd1 = d.getGrid(
      startTime = Some(LocalDateTime.parse("2020-12-08T00:00:00",FMT))
    )

    val dd2 = dd1.map( d => d)
    
    dd2.months.size should be > 0
    assert(dd2.months.size === dd1.months.size)
    

    for(i <- 0 to dd2.months.size -1 ) {
      assert(dd2.months(i).month === dd1.months(i).month)  

      for(j <- 0 to dd2.months(i).weeks.size -1 ) {
        assert(dd2.months(i).weeks(j) === dd1.months(i).weeks(j))

        for(n <- 0 to dd2.months(i).weeks(j).days.size -1 ) {
          assert(dd2.months(i).weeks(j).days(n) === dd1.months(i).weeks(j).days(n))
        
          assert(dd2.months(i).weeks(j).days(n).data === None)
          assert(dd1.months(i).weeks(j).days(n).data === None)
        }
      }
    }
  }

  "Github Daygrid for 8-Dec-2020" should "map with new data" in {
    val d = new DayGrid(tz = ZoneId.of("America/Los_Angeles"), locale = new Locale("en_US"))

    val dd1 = d.getGrid(
      startTime = Some(LocalDateTime.parse("2020-12-08T00:00:00",FMT))
    )

    val dd2 = dd1.map( d => d.copy(data = Some(scala.util.Random)))

    dd2.months.size should be > 0
    assert(dd2.months.size === dd1.months.size)
    

    for(i <- 0 to dd2.months.size -1 ) {
      assert(dd2.months(i).month === dd1.months(i).month)  

      for(j <- 0 to dd2.months(i).weeks.size -1 ) {
        assert(dd2.months(i).weeks(j) !== dd1.months(i).weeks(j))

        for(n <- 0 to dd2.months(i).weeks(j).days.size -1 ) {
          assert(dd2.months(i).weeks(j).days(n) !== dd1.months(i).weeks(j).days(n))
          
          assert(dd2.months(i).weeks(j).days(n).data !== None)
          assert(dd1.months(i).weeks(j).days(n).data === None)

          assert(dd2.months(i).weeks(j).days(n).data !== dd1.months(i).weeks(j).days(n).data)
        }
      }
    }
  }

  "Github Log" should "hit 3 days" in {
    val d = new DayGrid(tz = ZoneId.of("America/Los_Angeles"), locale = new Locale("en_US"))

    val dd1 = d.getGrid( past = 1,
      startTime = Some(LocalDateTime.parse("2020-12-09T13:00:00",FMT))
    )

    val data = Seq(
          "Date:   1607503394 +0200",
          "Date:   1607458267 +0200",
          "Date:   1607444734 +0200"
        )

    val dd2 = dd1.mapTimeHitsGithub(data)

    println(dd2)
    
    dd2.months.size should be > 0
    assert(dd2.months(1).weeks(0).days(0).data === Some(0) )
    assert(dd2.months(1).weeks(0).days(1).data === Some(0) )
    assert(dd2.months(1).weeks(0).days(2).data === Some(2) )
    assert(dd2.months(1).weeks(0).days(3).data === Some(1) )
    
  }

}