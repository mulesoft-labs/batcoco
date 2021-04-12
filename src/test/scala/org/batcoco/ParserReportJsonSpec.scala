package org.batcoco

import com.mulesoft.batcoco.mulesoft.batcoco.parser.BATGetRequests
import org.scalatest.{FunSuite, Matchers}

class ParserReportJsonSpec extends FunSuite with Matchers {

  lazy val URL = "https://devx.anypoint.mulesoft.com/monitoring/api/status"

  test("Parse report Json") {
    lazy val path =
      "data/json/reports/bat_report.json"
    val result = BATGetRequests.getAllRequests(path)
    println(result)
    result.requests.size should be(4)
  }

  test("Parse report Json case 2") {
    lazy val path = "data/json/reports/bat_report_20190216225055.json.full"
    val result = BATGetRequests.getAllRequests(path)
    println(result)
    result.requests.size should be(224)
  }
}
