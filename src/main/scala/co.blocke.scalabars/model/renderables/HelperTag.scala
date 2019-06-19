package co.blocke.scalabars
package model
package renderables

case class HelperTag(
    nameOrPath:  String,
    helper:      Helper,
    isInverted:  Boolean,
    arity:       Int,
    wsCtlBefore: Boolean,
    wsCtlAfter:  Boolean,
    expr:        ParsedExpression,
    blockParams: Seq[String],
    contents:    Seq[Renderable],
    wsAfter:     String
) extends BlockTag with Evalable {

  override def isBlock: Boolean = contents.nonEmpty

  override def render(rc: RenderControl): RenderControl =
    if (isBlock)
      super.render(rc)
    else helper match {
      case ph: PartialHelper =>
        // Special handling for PartialHelper, which replaces a non-block tag with a block tag.  Need to mock up a block
        // tag and render.
        println("HERE: " + ph) // TODO: PartialHelper's 't' is empty!  Shouldn't be!
        // TODO: Non-inline partials need open/close block wrappers w/ws to work!
        ph.t.compiled match {
          case openTag +: body :+ closeTag =>
            val otag = openTag.asInstanceOf[OpenTagProxy]
            val ctag = closeTag.asInstanceOf[CloseTagProxy]
            // convert this non-block into a block by "sewing" in template contents
            this.copy(contents = otag.copy(wsCtlBefore = wsCtlBefore) +: body :+ ctag.copy(wsCtlAfter = wsCtlAfter, wsAfter = wsAfter))
            super.render(rc) // now render partial's content as a block
        }
      case _ =>
        implicit val escapeOpts: Options = rc.opts.copy(_hash = rc.opts._hash + ("noEscape" -> BooleanEvalResult(!isEscaped)))
        val stage1 = checkFlush(rc.reset(), this)
        stage1.addContent(eval(stage1.opts))
    }

  def eval(options: Options): EvalResult[_] =
    if (nameOrPath == "@partial-block")
      options.context.lookup(nameOrPath).toEvalResult(options)
    else
      helper.run()(bakeOptions(options), Map.empty[String, Template])

  // Returns (fn, inverse) templates
  private def examineBlock(options: Options): (Template, Template) =
    if (contents.isEmpty)
      (EmptyTemplate(), EmptyTemplate())
    else
      contents.zipWithIndex.collectFirst {
        case (elseHelper: HelperTag, i) if elseHelper.nameOrPath == "else" =>
          val (fnT, invT) = contents.splitAt(i)

          // Unpack Else to see if there's an embedded 'if'.  If so... convert the Else into an If helper and add to _inverted
          if (elseHelper.expr.args.nonEmpty) {
            val newIf = elseHelper.copy(
              expr     = ParsedExpression(elseHelper.expr.args.head.value, elseHelper.expr.args.tail),
              contents = invT.tail.toList
            )
            (fnT, Seq(newIf))
          } else
            (fnT, invT)
      }.orElse(Some(contents, Seq.empty[Renderable])).map {
        case (fn, inv) if !isInverted => (SBTemplate(fn.toList, options), SBTemplate(inv.toList, options))
        case (fn, inv)                => (SBTemplate(inv.toList, options), SBTemplate(fn.toList, options))
      }.get

  // Do everything necessary to update Options before eval() on this helper.  Set up any current state
  // held in Options or to be passed down by parent.
  private def bakeOptions(options: Options): Options = {
    val (assignments, literalsAndPaths) = expr.args.partition(_.isInstanceOf[AssignmentArgument]).asInstanceOf[(Seq[AssignmentArgument], Seq[Argument])]

    // Poke assignments into Options.hash
    val hashAdds: Seq[(String, EvalResult[_])] = assignments.map(a => (a.label, a.targetValue.eval(options)))

    // Now eval all literals and paths and put EvalResults into params
    val evaledParams = literalsAndPaths.map(_.eval(options)).toList

    val (fn, inv) = examineBlock(options)

    // Cook the newly populated Options
    options.copy(
      helperName  = nameOrPath,
      blockParams = options.blockParams ++ blockParams.toList,
      params      = evaledParams,
      paramValues = literalsAndPaths.map(_.value).toList,
      _fn         = fn,
      _inverse    = inv,
      _hash       = options._hash ++ hashAdds.toMap)
  }

  override def toString(): String = {
    s"HelperTag $nameOrPath ($helper)\n" +
      "  args: " + expr.args + "\n" +
      "  contents: \n" +
      contents.map(_.toString).map(s => "    " + s).mkString("\n")
  }
}
