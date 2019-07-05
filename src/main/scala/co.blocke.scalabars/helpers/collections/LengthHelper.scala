package co.blocke.scalabars
package helpers.collections

import model._
import org.json4s._

case class LengthHelper() extends Helper("items") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    arg("items") match {
      case AsArray(a)  => a.arr.size
      case AsObject(o) => o.values.size
      case _           => 0
    }
}