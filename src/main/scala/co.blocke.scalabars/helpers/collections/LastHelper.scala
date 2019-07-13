package co.blocke.scalabars
package helpers.collections

import model._
import org.json4s.JArray

case class LastHelper() extends Helper("items") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    arg("items") match {
      case AsArray(a) if a.arr.nonEmpty =>
        (arg("items") >> LongEvalResult(a.arr.length - 1)).get
      case _ => ""
    }
}
