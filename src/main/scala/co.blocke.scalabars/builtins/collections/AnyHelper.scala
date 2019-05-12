package co.blocke.scalabars
package builtins.collections

import org.json4s._

case class AnyHelper() extends Helper(List("items")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    lookup("items").value match {
      case a: JArray if a.arr.nonEmpty     => options.fn()
      case o: JObject if o.values.nonEmpty => options.fn()
      case _                               => options.inverse()
    }
  }
}