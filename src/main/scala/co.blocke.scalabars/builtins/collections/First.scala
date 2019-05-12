package co.blocke.scalabars
package builtins.collections

import org.json4s.JArray

case class FirstHelper() extends Helper(List("items")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    val itemsContext = lookup("items")
    val arr = itemsContext.value.asInstanceOf[JArray].arr
    if (arr.nonEmpty)
      options.fn(lookup(s"items.[0]"))
    else
      options.inverse()
  }
}