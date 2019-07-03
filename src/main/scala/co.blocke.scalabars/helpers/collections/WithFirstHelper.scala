package co.blocke.scalabars
package helpers.collections

import model._

case class WithFirstHelper() extends Helper("items") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    arg("items") match {
      case AsArray(a) if a.arr.nonEmpty => options.fn((arg("items") >> LongEvalResult(0)).get)
      case _                            => options.inverse()
    }
}
