package co.blocke.scalabars
package helpers.stock

import model._

case class IfHelper() extends Helper("conditional") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    if (!options.isFalsy(arg("conditional")))
      options.fn()
    else
      options.inverse()
  }
}
