package co.blocke.scalabars
package model

import org.json4s._
import parsing.TagBuilder
import renderables._

case class PathHelper(path: String) extends Helper() {

  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] =

    (options.context.lookup(path), options._fn, options._inverse) match {
      case (c, EmptyTemplate(), EmptyTemplate()) => c.toEvalResult(options) // non-block path

      /* TODO...
      // If either array or object context create a synthetic each for this Handlebars behavior when it's a normal block label (non-partial, non-helper)
      case (c, fn, inv) if c.value.isInstanceOf[JObject] | c.value.isInstanceOf[JArray] =>
        val syntheticEach = TagBuilder(Whitespace(""), 3, expr = ParsedExpression("each", Seq(PathArgument(path))), body = fn.compiled ++ inv.compiled, trailingWS = Whitespace(""))
          .finalize(options.handlebars)(1).asInstanceOf[HelperTag]
        syntheticEach.eval(options)
        */

      case (c, _, _) if options.isFalsy(c)       => options.inverse(c)
      case (c, _, _)                             => options.fn(c)
    }
}
