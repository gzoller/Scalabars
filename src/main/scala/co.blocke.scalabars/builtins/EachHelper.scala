package co.blocke.scalabars
package builtins

import org.json4s.JArray

case class EachHelper() extends Helper(List("items")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    val itemsContext = lookup("items")
    val arr = itemsContext.value.asInstanceOf[JArray].arr
    if (arr.nonEmpty)
      arr.indices.map(i => options.fn(lookup(s"items.[$i]"))).mkString
    else
      options.inverse()
  }
}