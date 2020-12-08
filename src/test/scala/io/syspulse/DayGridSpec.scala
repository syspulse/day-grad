package io.syspulse

import org.scalatest._
import flatspec._
import matchers._

import java.time._
import java.time.format._
import java.time.temporal._
import java.util.Locale


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


    dd.months(0).month._2 shouldBe "JANUARY"
    dd.months(0).weeks(0).days(0).day._1 shouldBe 5
    
    dd.months(1).month._2 shouldBe "FEBRUARY"
    dd.months(1).weeks(0).days(0).day._1 shouldBe 2

    dd.months(2).month._2 shouldBe "MARCH"
    dd.months(2).weeks(0).days(0).day._1 shouldBe 1

    dd.months(3).month._2 shouldBe "APRIL"
    dd.months(3).weeks(0).days(0).day._1 shouldBe 5

    dd.months(4).month._2 shouldBe "MAY"
    dd.months(4).weeks(0).days(0).day._1 shouldBe 3

    dd.months(5).month._2 shouldBe "JUNE"
    dd.months(5).weeks(0).days(0).day._1 shouldBe 7

    dd.months(6).month._2 shouldBe "JULY"
    dd.months(6).weeks(0).days(0).day._1 shouldBe 5

    dd.months(7).month._2 shouldBe "AUGUST"
    dd.months(7).weeks(0).days(0).day._1 shouldBe 2

    dd.months(8).month._2 shouldBe "SEPTEMBER"
    dd.months(8).weeks(0).days(0).day._1 shouldBe 6

    dd.months(9).month._2 shouldBe "OCTOBER"
    dd.months(9).weeks(0).days(0).day._1 shouldBe 4

    dd.months(10).month._2 shouldBe "NOVEMBER"
    dd.months(10).weeks(0).days(0).day._1 shouldBe 1

    dd.months(11).month._2 shouldBe "DECEMBER"
    dd.months(11).weeks(0).days(0).day._1 shouldBe 6
  }

}