package co.blocke.scalabars
package builtins.collections

import org.json4s._

case class SortEachHelper() extends Helper(List("items")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    lookup("items").value match {
      case a: JArray =>
        a.arr.map(_.values.toString).sorted.map(s => options.fn(Context(JString(s)))).mkString("")
      case _ => ""
    }
}
