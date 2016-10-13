package controllers

import play.api.libs.json.{JsNumber, JsValue, Json}
import play.api.mvc._

class Application extends Controller {

  def hello() = Action(parse.json) { request =>
    val json: JsValue = request.body
    val number = (json \ "number").as[Int]
    val result : BigInt = factTree(number)
    val bdResult = BigDecimal.apply(result);
    Ok(Json.obj("result" -> JsNumber(bdResult)))
  }


  def prodTree(l: Int, r: Int): BigInt = {
    if (l > r) return 1
    if (l == r) return l
    if (r - l == 1) return BigInt.apply(l * r)
    val m: Int = (l + r) / 2
    prodTree(l, m) * prodTree(m + 1, r)
  }

  def factTree(n: Int): BigInt = {
    if (n < 0) return 0
    if (n == 0) return 1
    if (n == 1 || n == 2) return n
    prodTree(2, n)
  }


}