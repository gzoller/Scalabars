package co.blocke.scalabars
package builtins.collections

import org.json4s._

case class WithFirstHelper() extends Helper(List("items")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    lookup("items").value match {
      case a: JArray if a.arr.nonEmpty => options.fn(lookup("items.[0]"))
      case _                           => options.inverse()
    }
  }
}