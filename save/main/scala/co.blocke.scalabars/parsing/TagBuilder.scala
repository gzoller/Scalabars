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
    body:        Block            = EmptyBlock(),
    trailingWS:  Whitespace       = Whitespace("")
) extends Renderable {

  def render(rc: RenderControl): RenderControl = rc // will never be called... here to make parsing easier
  override def isBlock: Boolean = ctl.isDefined && List("#", "#*", "^", "#>").contains(ctl.get)

  // Break out whitespace and convert this builder into a final Tag based on its type
  def finalize(implicit sb: Scalabars): List[Renderable] = {
    val finalTag = ctl match {
      /*
      case Some("#*") =>
        val unpackedBody = contents.foldLeft(Seq.empty[Renderable]) {
          case (seq, item) =>
            item match {
              case tb: TagBuilder => seq ++ tb.finalize
              case r              => seq :+ r
            }
        }
        InlinePartialTag(expr.args.head, unpackedBody, wsCtlBefore, wsCtlAfter, trailingWS.ws) // inline partial
      case Some(">") | Some("#>") =>
        expr.exprArg match {
          // Sub-expression (expression in 1st position).  Create an ExpressionTag
          case Some(e) => ??? // TODO
          // Non-expression tag.  Use expr.name and build the PartialTag
          case None =>
            val partialHelper = sb.getPartial(expr.name) match {
              case Some(ph) => ph
              case None if isBlock =>
                // Partial not found, but fall-through block provided, so render that.
                PartialHelper(expr.name, sb._compileFromContents(contents))
              case None =>
                // Nothing found... this might be bad, or it may be a reference to an inline partial.  Won't know until render-time
                PartialHelper(expr.name, EmptyTemplate())
            }
            HelperTag(expr.name, partialHelper, isInverted = false, 3, wsCtlBefore, wsCtlAfter, expr, blockParams, contents, trailingWS.ws)
        }
        */

      case Some("#") => // Block Helper
        expr.exprArg match {
          // Sub-expression (expression in 1st position).  Create an ExpressionTag
//          case Some(e) => ???
          // Non-expression tag.  Use expr.name and build the HelperTag
          case None =>
            val helper = sb.getHelper(expr.name).getOrElse(throw new BarsException(s"Helper ${expr.name} is unknown."))
            BlockHelperTag(expr.name, helper, isInverted = false, expr, arity, blockParams, body)
        }

        /*
      case Some("^") => // Inverted Helper (fn & inverse are reversed, but otherwise the same as normal Helper)
        expr.exprArg match {
          // Sub-expression (expression in 1st position).  Create an ExpressionTag
          case Some(e) => ??? // TODO
          // Non-expression tag.  Use expr.name and build the HelperTag
          case None    => ??? // TODO
        }
        */

      case _ =>
        val helper = sb.getHelper(expr.name).getOrElse(PathHelper(expr.name)) // resolve to either known non-block helper, or fall back to assuming its a path
        HelperTag(expr.name, helper, 3, wsCtlBefore, wsCtlAfter, trailingWS.ws, expr)
    }
    List(leadingWS, finalTag, trailingWS)
  }
}

