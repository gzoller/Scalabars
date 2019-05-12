package co.blocke.scalabars
package builtins.collections

import org.json4s._

case class EmptyHelper() extends Helper(List("items")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    lookup("items").value match {
      case a: JArray if a.arr.isEmpty     => options.fn()
      case o: JObject if o.values.isEmpty => options.fn()
      case _                              => options.inverse()
    }
  }
}