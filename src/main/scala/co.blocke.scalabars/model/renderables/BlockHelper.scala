package co.blocke.scalabars
package model
package renderables

case class BlockHelper(
    name:        String,
    helper:      Helper,
    isInverted:  Boolean,
    expr:        ParsedExpression,
    arity:       Int,
    blockParams: Seq[String],
    body:        Block
) extends Renderable with Evalable with HelperTagCommon {

  def render(rc: RenderControl): RenderControl = {
    implicit val escapeOpts: Options = rc.opts.copy(_hash = rc.opts._hash + ("noEscape" -> BooleanEvalResult(true)))
    val raw: String = eval(escapeOpts)
    val wsTws = sliceToRenderables(raw)

    val stage1 = if (body.openTag.aloneOnLine) rc.clipLeading().clipTrailing() else rc
    val stage2 = if (body.openTag.wsCtlBefore) stage1.flushLeading() else stage1
    val stage3 = wsTws.foldLeft(stage2) {
      case (rcX, renderable) =>
        renderable.render(rcX)
    }
    val stage4 = if (body.closeTag.aloneOnLine) stage3.clipLeading().clipTrailing() else stage3
    val stage5 = if (body.closeTag.wsCtlAfter) stage4.flushTrailing() else stage4
    stage5
  }

  def eval(options: Options): EvalResult[_] = helper.run()(bakeBlockOptions(options), Map.empty[String, Template])

  // Blocks render their content bodies by calling options.fn().  Worse... the code of the helper may iterate, calling fn()
  // multiple times.  Whitespace must be managed each time, meaning we need to lift from the Helper.run() back up to the BlockHelperTag
  // context to manage that.  Use an anonymouse function/closure for this.
  val accumulator = (s: String, opts: Options) => {
    // Split rendered string into WS::Text::WS
    val wsTws = sliceToRenderables(s)
    val rc = RenderControl(opts)
    val stage1 = if (body.openTag.wsCtlAfter) rc.flushTrailing() else rc
    val stage2 = wsTws.foldLeft(stage1) { case (rcX, renderable) => renderable.render(rcX) }
    val stage3 = if (body.closeTag.wsCtlBefore) stage2.flushLeading() else stage2
    stage3.out.toString + stage3.accumulatedWS
  }

  // Do everything necessary to update Options before eval() on this helper.  Set up any current state
  // held in Options or to be passed down by parent.
  private def bakeBlockOptions(options: Options): Options = {
    val stage1Opts = bakeOptions(name, expr, options)

    val (fn, inv) = examineBlock(options)

    // Cook the newly populated Options
    stage1Opts.copy(
      blockParams = options.blockParams ++ blockParams.toList,
      accumulator = accumulator,
      _fn         = fn,
      _inverse    = inv)
  }

  private def examineBlock(options: Options): (Template, Template) =
    body.body.zipWithIndex.collectFirst {
      case (elseTag: BlockHelper, i) if elseTag.name == "else" =>
        val (fnT, invT) = body.body.splitAt(i)

        // Unpack Else to see if there's an embedded 'if'.  If so... convert the Else into an If helper and add to _inverted
        if (elseTag.expr.args.nonEmpty) {
          val newIf = elseTag.copy(
            expr = ParsedExpression(elseTag.expr.args.head.value, elseTag.expr.args.tail),
            body = body.copy(openTag = body.openTag.copy(expr = expr), body = invT.tail.toList)
          )
          (fnT, Seq(newIf))
        } else
          (body.openTag +: fnT :+ body.closeTag, body.openTag +: invT :+ body.closeTag)
    }.orElse(Some(body.body, Seq.empty[Renderable])).map {
      case (fn, inv) if !isInverted => (SBTemplate(fn.toList, options), SBTemplate(inv.toList, options))
      case (fn, inv)                => (SBTemplate(inv.toList, options), SBTemplate(fn.toList, options))
    }.get

  override def toString(): String = {
    s"BlockHelper $name ($helper)\n" +
      "  args: " + expr.args + "\n" +
      "  contents: \n" +
      body.body.map(_.toString).map(s => "    " + s).mkString("\n")
  }
}
