package co.blocke.scalabars
package helpers.collections

import model._
import org.json4s._

case class WithLookupHelper() extends Helper("thing", "loc") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    arg("thing") >> arg("loc") match {
      case Some(c) => options.fn(c)
      case None    => options.inverse()
    }
}
