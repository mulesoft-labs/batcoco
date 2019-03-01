package mulesoft.batcoco.mulesoft.batcoco.parser

import org.json4s._
import org.json4s.native.JsonMethods._

object BATGetRequests {

  implicit class ResourceUtil(path: String) {
    import scala.io.Source

    def getSource: String =
      Source
        .fromResource(path)
        .getLines()
        .mkString
  }

  def getAllRequests(path: String): BATTestResult = {
    try {
      val parsedJson = parse(path.getSource)
      val result = for {
        JObject(child) <- parsedJson
        JField("path", JString(url)) <- child
        JField("method", JString(method)) <- child
      } yield (url, method)
      BATTestResult(result)
    } catch {
      case e: Exception => throw new RuntimeException(e)
      case _: Throwable =>
        throw new RuntimeException("Exception trying to parse a json file.")
    }
  }
}
