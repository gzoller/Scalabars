package co.blocke.scalabars
package model

import org.json4s._
import helpers.stock.EachHelper
import renderables._

case class PathHelper(path: String) extends Helper() {

  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    (options.context.lookup(path), options._fn, options._inverse) match {
      //    options.context.lookup (path), options._fn, options._inverse) match {
      case (ctx, EmptyTemplate(), EmptyTemplate()) =>
        ctx.toEvalResult(options) // non-block path

      // If either array or object context create a synthetic each for this Handlebars behavior when it's a normal block label (non-partial, non-helper)
      case (ctx, fn, inv) if ctx.value.isInstanceOf[JArray] && options._fn != EmptyTemplate() =>
        val syntheticEach = BlockHelper(
          "each",
          EachHelper(false),
          isInverted = false,
          ParsedExpression("each", Seq(PathArgument(path))),
          3,
          Nil,
          Block(OpenTag(ParsedExpression("each", Seq(PathArgument(path))), false, false, 3), fn.compiled ++ inv.compiled, CloseTag(false, false, 3))
        )
        syntheticEach.eval(options) //.copy(context = c))

      case (ctx, _, _) if options.isFalsy(ctx) => options.inverse(ctx)
      case (ctx, _, _)                         => options.fn(ctx)
    }
  }
}
