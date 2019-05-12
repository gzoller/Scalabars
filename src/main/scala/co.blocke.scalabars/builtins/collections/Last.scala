package co.blocke.scalabars
package builtins.collections

import org.json4s.JArray

case class LastHelper() extends Helper(List("items")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    lookup("items").value match {
      case a: JArray if a.arr.nonEmpty => resolve(s"items.[0${a.arr.length - 1}]")
      case _                           => ""
    }
}