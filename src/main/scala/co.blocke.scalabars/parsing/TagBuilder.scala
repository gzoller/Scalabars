package co.blocke.scalabars
package parsing

import model._
import renderables._

case class TagBuilder(
    leadingWS:   Whitespace,
    arity:       Int,
    wsCtlBefore: Boolean          = false,
    ctl:         Option[String]   = None,
    expr:        ParsedExpression = ParsedExpression("nada"),
    blockParams: Seq[String]      = Seq.empty[String],
    wsCtlAfter:  Boolean          = false,
    body:        Block            = null,
    trailingWS:  Whitespace       = Whitespace("")
) extends Renderable {

  def render(rc: RenderControl): RenderControl =
    rc // will never be called... here to make parsing easier
  def isBlock: Boolean = ctl.isDefined && List("#", "#*", "^", "#>").contains(ctl.get)

  // Break out whitespace and convert this builder into a final Tag based on its type
  def finalize(implicit sb: Scalabars): List[Renderable] = {
    val finalTag = ctl match {
      case Some("#*") =>
        InlinePartialTag(expr.args.head, body) // inline partial

      case Some(">") | Some("#>") =>
        expr.exprArg match {
          // Sub-expression (expression in 1st position).  Create an ExpressionTag
          case Some(e) => ??? // TODO: subexpression
          // Non-expression tag.  Use expr.name and build the PartialTag
          case None =>
            val partialHelper = sb.getPartial(expr.name) match {
              case Some(ph) => ph
              case None => // Nothing found... this might be bad, or it may be a reference to an inline partial.  Won't know until render-time
                PartialHelper(expr.name, EmptyTemplate())
            }
            if (isBlock)
              BlockHelper(expr.name, partialHelper, isInverted = false, expr, 3, blockParams, body)
            else
              HelperTag(
                expr.name,
                partialHelper,
                expr,
                wsCtlBefore,
                wsCtlAfter,
                3,
                Some(leadingWS.ws.reverse.takeWhile(_ != '\n').reverse))
        }

      case Some("#") => // Blocks
        expr.exprArg match {
          // Sub-expression (expression in 1st position).  Create an ExpressionTag
          case Some(e) => ??? // TODO
          // Non-expression tag.  Use expr.name and build the HelperTag
          case None =>
            val helper = sb.getHelper(expr.name).getOrElse(PathHelper(expr.name))
            BlockHelper(expr.name, helper, isInverted = false, expr, arity, blockParams, body)

        }

      case Some("^") => // Inverted Helper (fn & inverse are reversed, but otherwise the same as normal Helper)
        expr.exprArg match {
          // Sub-expression (expression in 1st position).  Create an ExpressionTag
          case Some(e) => ??? // TODO
          // Non-expression tag.  Use expr.name and build the HelperTag
          case None =>
            val helper = sb.getHelper(expr.name).getOrElse(PathHelper(expr.name))
            BlockHelper(expr.name, helper, isInverted = true, expr, arity, blockParams, body)
        }

      case _ =>
        val helper = sb.getHelper(expr.name).getOrElse(PathHelper(expr.name)) // resolve to either known non-block helper, or fall back to assuming its a path
        HelperTag(expr.name, helper, expr, wsCtlBefore, wsCtlAfter, arity, None)
    }
    List(leadingWS, finalTag, trailingWS)
  }
}
