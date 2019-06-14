package co.blocke.scalabars
package helpers.comparison

import model._

case class AndHelper() extends Helper() {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    if (options.params.map(p => !options.isFalsy(p)).forall(_ == true))
      options.fn()
    else
      options.inverse()
}

