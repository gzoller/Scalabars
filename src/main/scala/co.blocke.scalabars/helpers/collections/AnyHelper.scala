package co.blocke.scalabars
package helpers.collections

import model._
import org.json4s._

case class AnyHelper() extends Helper("items") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    arg("items") match {
      case AsArray(a) if a.arr.nonEmpty     => options.fn()
      case AsObject(o) if o.values.nonEmpty => options.fn()
      case _                                => options.inverse()
    }
}