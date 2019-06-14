package co.blocke.scalabars
package helpers.collections

import model._
import org.json4s._

case class LengthEqualsHelper() extends Helper("items", "len") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    val size = arg("items") match {
      case AsArray(a)  => a.arr.size
      case AsObject(o) => o.values.size
      case _           => 0
    }
    scalarArg("len") match {
      case Some(len) if size == len =>
        options.fn()
      case _ =>
        options.inverse()
    }
  }
}