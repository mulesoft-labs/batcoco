package org.batcoco

import amf.client.model.domain.WebApi
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class BatReportTest extends FunSuite with Matchers {

  test("Simple BAT Report") {

    val url = getClass.getResource("/bat-report.json")
    println(url)

    // parse
    val json = Source.fromFile(url.toURI)
    val mapper = new ObjectMapper() with ScalaObjectMapper
    mapper.registerModule(DefaultScalaModule)

    val parsedJson = mapper.readValue[Map[String, Object]](json.reader())
    parsedJson.foreach(entry => {
      println(entry._1 + "=" + entry._2)
    })

 /*  val report = mapper.readValue[BatReportTest](json.reader())
   println("YES " + report.getClass)
 */
  }


  def dump(text : String, webApi : WebApi): Unit = {

    println("------ " + text + " ------")
    webApi.endPoints.forEach(endPoint => {
        println(endPoint.path.value())
        endPoint.operations.forEach(operation => println("\t" + operation.method.value()))
    })
    println("--------------------------------")
  }

}
