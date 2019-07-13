package co.blocke.scalabars
package helpers.comparison

import model._

case class OrHelper() extends Helper("a", "b") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    if (options.params.map(p => !options.isFalsy(p)).foldLeft(false)(_ || _))
      options.fn()
    else
      options.inverse()
}