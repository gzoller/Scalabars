package co.blocke.scalabars
package helpers.collections

import model._

case class FirstHelper() extends Helper("items") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    arg("items") match {
      case AsArray(a) if a.arr.nonEmpty =>
        val z = arg("items") >> LongEvalResult(0)
        if (z.isDefined) z.get else ""
      case _ => ""
    }
}