package co.blocke.scalabars
package helpers.collections

import model._
import org.json4s._

case class JoinHelper() extends Helper("items", "sep") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    val sep = scalarArg("sep").map { _.toString }.getOrElse("")
    arg("items") match {
      case AsArray(a) => a.arr.map(_.values.toString).mkString(sep)
      case _          => ""
    }
  }
}