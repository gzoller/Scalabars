package co.blocke.scalabars
package helpers.stock

import model._
import org.json4s._

case class WithHelper() extends Helper("target") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =
    arg("target") match {
      case pre: ContextEvalResult =>
        val newOpts = options.copy(context = pre.value)
        pre.value.value match {
          case JNothing                           => newOpts.inverse()
          case jo: JObject if jo.children.isEmpty => newOpts.inverse()
          case a: JArray if a.arr.isEmpty         => newOpts.inverse()
          case s: JString if s.values == "false" =>
            println("Yip")
            newOpts.inverse()
          case _ if options.blockParams.isEmpty => newOpts.fn(pre.value)
          case _ if options.blockParams.isEmpty =>
            newOpts.fn(pre.value, Map.empty[String, EvalResult[_]], Map(options.blockParams.head -> pre.value))
        }
      case NoEvalResult()           => options.inverse()
      case BooleanEvalResult(true)  => options.fn()
      case BooleanEvalResult(false) => options.inverse()
      case e: EvalResult[_] =>
        implicit val ctx: Context = options.context // trigger implicit conversion
        if (options.blockParams.isEmpty)
          options.fn(e)
        else
          options.fn(ctx, Map.empty[String, EvalResult[_]], Map(options.blockParams.head -> ctx))
    }
}
