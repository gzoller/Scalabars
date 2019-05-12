package co.blocke.scalabars
package builtins.collections

import org.json4s._

case class EachIndexHelper() extends Helper(List("items")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    lookup("items").value match {
      case a: JArray if a.arr.nonEmpty =>
        a.arr.indices.map { i =>
          val ctx = lookup(s"items.[$i]")
          options.fn(ctx.copy(extras = ctx.extras + ("index" -> Context(JInt(i)))))
        }.mkString
      case _ => options.inverse()
    }
}