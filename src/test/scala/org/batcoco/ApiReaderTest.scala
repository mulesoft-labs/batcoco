package org.batcoco

import java.net.URI
import java.util.regex.PatternSyntaxException

import amf.client.AMF
import amf.client.model.document.Document
import amf.client.model.domain.WebApi
import amf.plugins.features.validation.AMFValidatorPlugin
import mulesoft.batcoco.EndpointItem
import mulesoft.batcoco.mulesoft.batcoco.parser.{
  BATGetRequests,
  BATTestResult,
  Batcoco
}
import mulesoft.batcoco.mulesoft.batcoco.service.BatcocoService
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}

import scala.collection.JavaConverters._
import scala.util.matching.Regex

class ApiReaderTest extends FunSuite with Matchers with BeforeAndAfterAll {

  override def beforeAll {
    try {
      AMF.init.get
      AMFValidatorPlugin.withEnabledValidation(false)
    } catch {
      case e: Exception =>
        e.printStackTrace()
    }
  }

  ignore("Simple RAML") {

    val url = getClass.getResource("/example.raml")

    val parser = AMF.ramlParser()
    val baseUnit = parser.parseFileAsync(url.toString).get()
    val document = AMF.resolveRaml10(baseUnit).asInstanceOf[Document]
    val webApi = document.encodes.asInstanceOf[WebApi]

    val endPoints = webApi.endPoints
    endPoints.size() should be(5)

    dump("example.raml", webApi)
  }

  ignore("Simple OAS") {

    val url = getClass.getResource("/example.json")

    val parser = AMF.oas20Parser()
    val baseUnit = parser.parseFileAsync(url.toString).get()
    val document = AMF.resolveOas20(baseUnit).asInstanceOf[Document]
    val webApi = document.encodes.asInstanceOf[WebApi]

    val endPoints = webApi.endPoints
    endPoints.size() should be(5)
    dump("example.json", webApi)

  }

  test("BatCoco get Case class with a Map test") {

    val url = getClass.getResource("/example.raml")

    val parser = AMF.ramlParser()
    val baseUnit = parser.parseFileAsync(url.toString).get()
    val document = AMF.resolveRaml10(baseUnit).asInstanceOf[Document]
    val webApi = document.encodes.asInstanceOf[WebApi]

    val endPoints = webApi.endPoints
    endPoints.size() should be(5)

    val results = dump("example.raml", webApi)
    assert(results.size == 6)
    println(results)
  }

  test("test if exists some URL") {

    val url = getClass.getResource("/example.raml")

    val parser = AMF.ramlParser()
    val baseUnit = parser.parseFileAsync(url.toString).get()
    val document = AMF.resolveRaml10(baseUnit).asInstanceOf[Document]
    val webApi = document.encodes.asInstanceOf[WebApi]

    val endPoints = webApi.endPoints
    endPoints.size() should be(5)

    val endpointItems = dump("example.raml", webApi)
    val path = "/resources"
    val method = "GET"
    val endpointItem = find(path, method, endpointItems)
    assert(endpointItem.get.url == path)
    assert(endpointItem.get.method == method)

  }

  test("check build REGEX") {

    val resourcePath = "/part/{uriParam1}/{uriParam2}/{uriParam3}"
    val path = "/part/123/1231/12313"
    val result = matchesResource(resourcePath, path)
    assert(result)

    val path2 = "/part/123/1231/"
    val result2 = matchesResource(resourcePath, path2)
    assert(result2)

    val path3 = "/part/12313/123/297834923-123123-asdfads"
    val result3 = matchesResource(resourcePath, path3)
    assert(result3)
  }

  test("check build REGEX2") {

    val resourcePath = "/part/{uriParam1}/{uriParam2}"
    val path = "/part/123/1231"
    val result = matchesResource(resourcePath, path)
    assert(result)

    val path2 = "/part/123/asdfasdf?"
    val result2 = matchesResource(resourcePath, path2)
    assert(result2)

    val path3 = "/part/asdfasdf/adsfasdf"
    val result3 = matchesResource(resourcePath, path3)
    assert(result3)
  }

  def find(path: String,
           method: String,
           endpointItems: List[EndpointItem]): Option[EndpointItem] = {
    endpointItems.find(ei =>
      ei.url.matches(path) && ei.method.equalsIgnoreCase(method))
  }

  def dump(text: String, webApi: WebApi): List[EndpointItem] = {

    println("------ " + text + " ------")
    val results = webApi.endPoints.asScala.map(endPoint => {
      //        println(endPoint.path.value())
      val result = endPoint.operations.asScala.map(operation => {
        //          println("\t" + operation.method.value())
        EndpointItem(url = endPoint.path.value(),
                     method = operation.method.value().toUpperCase)
      })
      //        println("--------------------------------")
      result.toList
    })
    results.toList.flatten

  }

  private def matchesResource(resourcePath: String, path: String): Boolean = {
    buildPathRegex(resourcePath, exactMatch = true) match {
      case Some(r) =>
        r.findFirstIn(path).isDefined
      case None =>
        // TODO return an error
        false
    }
  }

  val Separator: String = "/"

  def buildPathRegex(path: String,
                     exactMatch: Boolean = false): Option[Regex] = {
    try {
      // If path is /, we don't need to make a regex
      val regex: String =
        if (path.equals(Separator))
          s"\\$Separator"
        else {
          path
            .split(Separator)
            .toList
            .map { s =>
              //              if(s.startsWith("{") && s.endsWith("}")) "[\\w\\S]+" else s
              // TODO check which regex should be used
              if (path.endsWith(Separator)) {
                s.replaceAll("\\{[^\\/\\{\\}]+\\}", "([^\\/]+)?\\/{1}")
              } else {
                s.replaceAll("\\{[^\\/\\{\\}]+\\}", "([^\\/]+)?")
              }
            }
            .mkString(s"\\$Separator")

        }
      if (exactMatch) Some(s"^$regex$$".r) else Some(s"^$regex".r)
    } catch {
      case e: PatternSyntaxException =>
        // TODO how to log the error ?
        //        logError(s"An error occured while trying to create regular expression for Path $path")
        None
    }
  }

  test("Coverage") {

    val url = getClass.getResource("/raml/batcoco_v2.raml")

    val parser = AMF.ramlParser()
    val baseUnit = parser.parseFileAsync(url.toString).get()
    val document = AMF.resolveRaml10(baseUnit).asInstanceOf[Document]
    val webApi = document.encodes.asInstanceOf[WebApi]

    lazy val path = "data/json/reports/bat_report.json"
    val result = BATGetRequests.getAllRequests(path)
    val endPoints = webApi.endPoints

    val batcoco = new Batcoco(result)
    endPoints.asScala.map(endPoint => {

      val basePath =
        webApi.servers.asScala.headOption.map(server => server.url.value())
      val path =
        if (basePath.isDefined) basePath.get + endPoint.path.value()
        else endPoint.path.value()

      val resultCoverage = endPoint.operations.asScala.map(op => {
        val method = op.method.value()
        val tested = batcoco.tested(path, method)
        if (!tested) {
          println(s"NO tested $path $method")
        } else {
          println(s"tested OK $path $method")
        }
        (path, method, tested)
      })
      resultCoverage
    })

  }

  test("Check Service BatcocoService") {

    lazy val pathRAML = "/raml/batcoco_v2.raml"
    lazy val pathReport = "data/json/reports/bat_report.json"
    val result = BatcocoService.process(pathRAML, pathReport)

    println(
      f"Coverage ${result.count(r => r._3 === true).toDouble / result.size}%1.2f")
  }

}
