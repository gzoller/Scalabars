package co.blocke.scalabars
package helpers.misc

import model._
import org.json4s._

case class DefaultHelper() extends Helper("lookup", "fallback") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    arg("lookup") match {
      case c: ContextEvalResult => c.value.value match {
        case JNothing | JNull => arg("fallback")
        case _                => c
      }
    }
  }
}