package org.batcoco

import amf.client.AMF
import amf.client.model.document.Document
import amf.client.model.domain.WebApi
import amf.plugins.features.validation.AMFValidatorPlugin
import mulesoft.batcoco.EndpointItem
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}

import scala.collection.JavaConverters._
import scala.collection.mutable

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

  def find(path: String, method: String, endpointItems: List[EndpointItem]): Option[EndpointItem] = {
    endpointItems.find(ei => ei.url.matches(path) && ei.method.equalsIgnoreCase(method))
  }

  def dump(text : String, webApi : WebApi): List[EndpointItem] = {

    println("------ " + text + " ------")
    val results = webApi.endPoints.asScala.map( endPoint => {
//        println(endPoint.path.value())
        val result =endPoint.operations.asScala.map( operation => {
//          println("\t" + operation.method.value())
          EndpointItem(url = endPoint.path.value(),method = operation.method.value().toUpperCase )
        })
//        println("--------------------------------")
        result.toList
    })
    results.toList.flatten

  }
}
