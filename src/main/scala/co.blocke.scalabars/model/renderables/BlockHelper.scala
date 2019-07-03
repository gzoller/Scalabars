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
) extends Renderable
  with Evalable
  with HelperTagCommon {

  def render(rc: RenderControl): RenderControl = {
    implicit val escapeOpts: Options =
      rc.opts.copy(_hash = rc.opts._hash + ("noEscape" -> BooleanEvalResult(true)))
    eval(escapeOpts) match {
      case r: RenderableEvalResult => // Hey!  We've become something else (PartialHelper)
        r.value.render(rc)
      case e =>
        val raw: String = e
        rc.addText(raw)
    }
  }

  def eval(options: Options): EvalResult[_] =
    helper.run()(bakeBlockOptions(options), Map.empty[String, Template])

  // Do everything necessary to update Options before eval() on this helper.  Set up any current state
  // held in Options or to be passed down by parent.
  private def bakeBlockOptions(options: Options): Options = {
    val stage1Opts = bakeOptions(name, expr, options)

    val (fn, inv) = examineBlock(options)

    // Cook the newly populated Options
    stage1Opts.copy(blockParams = options.blockParams ++ blockParams.toList, _fn = fn, _inverse = inv)
  }

  private def examineBlock(options: Options): (Template, Template) =
    body.body.zipWithIndex
      .collectFirst {
        case (elseTag: HelperTag, i) if elseTag.nameOrPath == "else" =>
          val (fnT, invT) = body.body.splitAt(i)

          // Unpack Else to see if there's an embedded 'if'.  If so... convert the Else into an If helper and add to _inverted
          if (elseTag.expr.args.nonEmpty) {
            val newIf = BlockHelper(
              "if",
              helper,
              false,
              ParsedExpression(elseTag.expr.args.head.value, elseTag.expr.args.tail),
              arity,
              blockParams,
              body.copy(openTag = body.openTag.copy(expr = expr), body = invT.tail.toList)
            )
            (fnT, Seq(newIf))
          } else
            (body.openTag +: fnT :+ body.closeTag, body.openTag +: invT :+ body.closeTag)
      }
      .orElse(Some(body.body, Seq.empty[Renderable]))
      .map {
        case (fn, inv) if !isInverted =>
          (SBTemplate(fn.toList, options), SBTemplate(inv.toList, options))
        case (fn, inv) => (SBTemplate(inv.toList, options), SBTemplate(fn.toList, options))
      }
      .get

  override def toString(): String = {
    s"BlockHelper $name ($helper)\n" +
      "  args: " + expr.args + "\n" +
      "  contents: \n" +
      body.body.map(_.toString).map(s => "    " + s).mkString("\n") +
      "\n--> (end BlockHelper)"
  }
}
