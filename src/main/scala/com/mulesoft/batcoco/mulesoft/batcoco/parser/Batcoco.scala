package com.mulesoft.batcoco.mulesoft.batcoco.parser

import java.util.regex.PatternSyntaxException

import scala.util.matching.Regex

class Batcoco(result: BATTestResult) {

  val Separator: String = "/"

  // Path is RAML BaseUri + Endpoint Path
  def tested(path: String, method: String): Boolean = {

    result.requests.foreach(test => {
      val runtimePath = test._1
      if (matchesResource(path, runtimePath) && test._2.equalsIgnoreCase(
            method)) return true
    })
    false
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

  private def buildPathRegex(path: String,
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
}
