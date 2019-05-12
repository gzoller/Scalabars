package co.blocke.scalabars
package builtins.collections

import org.json4s._

case class ContainsHelper() extends Helper(List("items", "target")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    lookup("items").value match {
      case a: JArray if a.values.contains(resolve("target")) => options.fn()
      case o: JObject if o.values.contains(resolve("target")) => options.fn()
      case _ => options.inverse()
    }
}