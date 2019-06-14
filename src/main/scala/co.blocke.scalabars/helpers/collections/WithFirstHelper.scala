package co.blocke.scalabars
package helpers.collections

import model._
import org.json4s._

case class WithFirstHelper() extends Helper("items") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    arg("items") match {
      case AsArray(a) if a.arr.nonEmpty => arg("items") >> LongEvalResult(0) match {
        case Some(c) => options.fn(c)
        case None    => options.inverse()
      }
      case _ => options.inverse()
    }
}