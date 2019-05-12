package co.blocke.scalabars
package builtins.collections

import org.json4s.JArray

case class WithBeforeHelper() extends Helper(List("items", "loc")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    val loc = resolve("loc").toInt
    lookup("items").value match {
      case a: JArray => a.arr.dropRight(loc).indices.map(i => options.fn(lookup(s"items.[$i]"))).mkString
      case _         => options.inverse()
    }
  }
}