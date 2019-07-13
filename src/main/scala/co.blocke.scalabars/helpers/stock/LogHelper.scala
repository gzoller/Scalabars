package co.blocke.scalabars
package helpers.stock

import model._

case class LogHelper() extends Helper() {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    val logLevel = options.hash("level") match {
      case ""    => "info"
      case level => level
    }
    val msg = options.params
      .map { e =>
        val s: String = e
        s
      }
      .mkString(" ")
    Handlebars(options.handlebars).log(logLevel, msg)
    ""
  }
}
