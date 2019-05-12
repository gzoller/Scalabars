package co.blocke.scalabars
package builtins.collections

import org.json4s._

case class LengthEqualsHelper() extends Helper(List("items", "len")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    val size = lookup("items").value match {
      case a: JArray  => a.arr.size
      case o: JObject => o.values.size
      case _          => 0
    }
    if (size == resolve("len").toInt)
      options.fn()
    else
      options.inverse()
  }
}