package mulesoft.batcoco.mulesoft.batcoco.service

import amf.client.AMF
import amf.client.model.document.Document
import amf.client.model.domain.WebApi
import mulesoft.batcoco.mulesoft.batcoco.parser.{BATGetRequests, Batcoco}

import scala.collection.JavaConverters._

object BatcocoService {

  def process(pathRAML: String,
              pathResult: String): List[(String, String, Boolean)] = {
    val url = getClass.getResource(pathRAML)

    val parser = AMF.ramlParser()
    val baseUnit = parser.parseFileAsync(url.toString).get()
    val document = AMF.resolveRaml10(baseUnit).asInstanceOf[Document]
    val webApi = document.encodes.asInstanceOf[WebApi]

    val result = BATGetRequests.getAllRequests(pathResult)
    val endPoints = webApi.endPoints

    val batCoco = new Batcoco(result)
    endPoints.asScala
      .flatMap(endPoint => {
        val basePath =
          webApi.servers.asScala.headOption.map(server => server.url.value())
        val path =
          if (basePath.isDefined) basePath.get + endPoint.path.value()
          else endPoint.path.value()

        val resultCoverage = endPoint.operations.asScala
          .map(op => {
            val method = op.method.value()
            val tested = batCoco.tested(path, method)
            if (!tested) {
              println(s"NO tested $path $method")
            } else {
              println(s"tested OK $path $method")
            }
            (path, method, tested)
          })
          .toList
        resultCoverage
      })
      .toList
  }
}
