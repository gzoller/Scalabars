package co.blocke.scalabars
package builtins.collections

import org.json4s.JArray

case class LastHelper() extends Helper(List("items")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    val itemsContext = lookup("items")
    val arr = itemsContext.value.asInstanceOf[JArray].arr
    if (arr.nonEmpty)
      options.fn(lookup(s"items.[${arr.length - 1}]"))
    else
      options.inverse()
  }
}