package co.blocke.scalabars
package builtins.collections

import org.json4s._

case class LengthHelper() extends Helper(List("items")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    lookup("items").value match {
      case a: JArray  => a.arr.size.toString
      case o: JObject => o.values.size.toString
      case _          => "0"
    }
}