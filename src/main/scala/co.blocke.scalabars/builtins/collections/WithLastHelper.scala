package co.blocke.scalabars
package builtins.collections

import org.json4s._

case class WithLastHelper() extends Helper(List("items")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    lookup("items").value match {
      case a: JArray if a.arr.nonEmpty => options.fn(lookup(s"items.[${a.arr.length - 1}]"))
      case _                           => options.inverse()
    }
  }
}