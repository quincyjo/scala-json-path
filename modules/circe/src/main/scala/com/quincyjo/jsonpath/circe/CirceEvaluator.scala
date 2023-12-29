package com.quincyjo.jsonpath.circe

import com.quincyjo.jsonpath.JsonPathEvaluator
import com.quincyjo.jsonpath.circe.implicits.circeSupport
import io.circe.Json

object CirceEvaluator extends JsonPathEvaluator[Json]
