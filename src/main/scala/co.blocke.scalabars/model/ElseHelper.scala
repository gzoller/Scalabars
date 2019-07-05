package co.blocke.scalabars
package model

case class ElseHelper() extends Helper() {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = ""
}