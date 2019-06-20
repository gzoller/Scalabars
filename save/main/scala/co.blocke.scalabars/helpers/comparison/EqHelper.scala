package co.blocke.scalabars
package helpers.comparison

import model._

case class EqHelper() extends Helper("a", "b") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    if (normalizeResult(arg("a")) == normalizeResult(arg("b")))
      options.fn()
    else
      options.inverse()
  }

}
