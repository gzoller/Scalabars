package co.blocke.scalabars
package model
package renderables

case class HelperTag(
    nameOrPath:  String,
    helper:      Helper,
    expr:        ParsedExpression, // "guts" of the tag (e.g. label, arguments)
    wsCtlBefore: Boolean,
    wsCtlAfter:  Boolean,
    arity:       Int,
    aloneOnLine: Boolean          = false
) extends Tag with Evalable with HelperTagCommon {

  def render(rc: RenderControl): RenderControl = {
    implicit val escapeOpts: Options = rc.opts.copy(_hash = rc.opts._hash + ("noEscape" -> BooleanEvalResult(!isEscaped)))
    eval(escapeOpts) match {
      case r: RenderableEvalResult => // Hey!  We've become something else (a BlockHelper in fact).  Replace ourselves and re-render!
        // Have no idea why, but you have to invalidate aloneOnLine on open tag if wsctl present.  HB works this way...
        // Also wire the {{> tag}} wsCtl before to the block's open tag wsctlafter
        val fixed = r.value match {
          case bh: BlockHelper =>
            if ((bh.body.openTag.wsCtlBefore || bh.body.openTag.wsCtlAfter) && aloneOnLine)
              bh.copy(body = bh.body.copy(
                openTag = bh.body.openTag.copy(aloneOnLine = false)))
            else
              bh
          case _ => r.value // Should Never Happen(tm)
        }
        val stage0 = if (wsCtlBefore) rc.flushLeading() else rc
        val stage1 = fixed.render(stage0).reset()
        val stage2 = if (aloneOnLine) stage1.clipTrailing() else stage1
        if (wsCtlAfter) stage2.flushTrailing() else stage2
      case e =>
        val body = sliceToRenderables(e)
        val stage1 = if (wsCtlBefore) rc.flushLeading() else rc
        val stage2 = body.foldLeft(stage1) { case (rcX, renderable) => renderable.render(rcX) }
        if (wsCtlAfter) stage2.flushTrailing() else stage2
    }
  }

  def eval(options: Options): EvalResult[_] =
    if (nameOrPath == "@partial-block")
      options.context.lookup(nameOrPath).toEvalResult(options)
    else
      helper.run()(bakeOptions(nameOrPath, expr, options), Map.empty[String, Template])

  override def toString(): String = {
    s"HelperTag $nameOrPath ($helper)\n" +
      "  args: " + expr.args + "\n"
  }
}
