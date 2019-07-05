package co.blocke.scalabars
package helpers.misc

import model._

case class RawHelper() extends Helper() {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = options.fn()
}
