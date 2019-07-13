package co.blocke.scalabars
package helpers.collections

import model._
import org.json4s._

case class ContainsHelper() extends Helper("items", "target") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    val jvTarget: JValue = arg("target")
    arg("items") match {
      case AsArray(a) if a.values.contains(jvTarget.values) => options.fn()
      case AsObject(o) if o.values.contains(jvTarget.values.toString) => options.fn()
      case _ => options.inverse()
    }
  }
}