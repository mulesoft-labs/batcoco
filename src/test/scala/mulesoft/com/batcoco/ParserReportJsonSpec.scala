package mulesoft.com.batcoco

import mulesoft.com.batcoco.json.BATGetRequests
import org.scalatest.{FlatSpec, Matchers}

class ParserReportJsonSpec extends FlatSpec with Matchers {

  lazy val URL = "https://devx.anypoint.mulesoft.com/monitoring/api/status"

  it should "Parse report Json" in {
    lazy val path =
      "data/json/bat_report_20190216225055.json"
    val result = BATGetRequests.getAllRequests(path)
    println(result)
    result.requests.size should be(2)
  }

  it should "Parse report Json case 2" in {
    lazy val path = "data/json/bat_report_20190216225055.json.full"
    val result = BATGetRequests.getAllRequests(path)
    println(result)
    result.requests.size should be(224)
  }
}
