package co.blocke.scalabars
package builtins.collections

import org.json4s._

case class JoinHelper() extends Helper(List("items", "sep")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    val sep = lookup("sep").value match {
      case s: JString => s.values
      case _          => ""
    }
    lookup("items").value match {
      case a: JArray => a.arr.map(_.values.toString).mkString(sep)
      case _         => ""
    }
  }
}