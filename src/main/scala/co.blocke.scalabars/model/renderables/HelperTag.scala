package co.blocke.scalabars
package model
package renderables

case class HelperTag(
    nameOrPath:     String,
    helper:         Helper,
    expr:           ParsedExpression, // "guts" of the tag (e.g. label, arguments)
    wsCtlBefore:    Boolean,
    wsCtlAfter:     Boolean,
    arity:          Int,
    aloneOnLine:    Boolean          = false, // needed for indent of partial block if helper eval is RenderableEvalResult
    savedLeadingWS: Option[String] // for {{> tag}}  Need to save preceeding ws to indent partial block
) extends Tag
  with Evalable
  with HelperTagCommon {

  def render(rc: RenderControl): RenderControl = {
    implicit val escapeOpts: Options =
      rc.opts.copy(_hash = rc.opts._hash + ("noEscape" -> BooleanEvalResult(!isEscaped)))
    eval(escapeOpts) match {
      case r: RenderableEvalResult => // Hey!  We've become something else (a BlockHelper in fact).  Replace ourselves and re-render!
        // Have no idea why, but you have to invalidate aloneOnLine on open tag if wsctl present.  HB works this way...
        // Also wire the {{> tag}} wsCtl before to the block's open tag wsctlafter
        val stage0 = if (wsCtlBefore) rc.flushLeading() else rc

        val subRC = RenderControl(escapeOpts)
        val raw = r.value.render(subRC).out.toString

        // Prepend ws before > tag to each line of block (partial) output
        if (aloneOnLine && rc.opts.hash("preventIndent") != "true" && !wsCtlBefore) {
          val indent = savedLeadingWS.getOrElse("")
          val bumped =
            raw.split("\n", -1).map(line => if (line != "") indent + line else line).mkString("\n")
          rc.addText(bumped)
        } else
          rc.addText(raw)
      case e =>
        val raw: String = e
        rc.addText(raw)
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
