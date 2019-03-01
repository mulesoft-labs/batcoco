package com.mulesoft.batcoco.mulesoft.batcoco.service

trait BatcocoService {

  def process(pathRAML: String,
              pathResult: String): List[(String, String, Boolean)]
}
