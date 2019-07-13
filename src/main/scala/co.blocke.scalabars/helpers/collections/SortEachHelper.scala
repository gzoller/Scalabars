package co.blocke.scalabars
package helpers.collections

import model._
import org.json4s._

case class SortEachHelper() extends Helper("items") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    arg("items") match {
      case AsArray(a) =>
        a.arr.map(_.values.toString).sorted.map(s => options.fn(options.context.push(JString(s), "$sortedLiteral"))).mkString("")
      case _ => ""
    }
}
