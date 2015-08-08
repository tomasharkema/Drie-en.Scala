package helpers

import play.api.libs.json.{JsValue, Json}

/**
 * Created by tomas on 08-08-15.
 */
object JsonHelpers {
  def objToString(js: JsValue) = Json.stringify(js)
}
