package co.blocke.scalabars
package builtins.collections

import org.json4s._

case class EachIndexHelper() extends Helper(List("items")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    val itemsContext = lookup("items")
    val arr = itemsContext.value.asInstanceOf[JArray].arr
    if (arr.nonEmpty)
      arr.indices.map { i =>
        val ctx = lookup(s"items.[$i]")
        options.fn(ctx.copy(extras = ctx.extras + ("index" -> Context(JInt(i)))))
      }.mkString
    else
      options.inverse()
  }
}