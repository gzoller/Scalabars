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
        val renderedRC = r.value.render(rc).reset()
        if (aloneOnLine) renderedRC.clipTrailing() else renderedRC
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
