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
    aloneOnLine: Boolean
) extends BlockTag with Evalable {

  override def isBlock: Boolean = contents.nonEmpty

  def render(options: Options): (Options, String) = {
    // Since we implicitly convert EvalResult->String, we need to artificially poke Options to tell implicit convert
    // whether or not to HTML-escape non-safe Strings, per Handlebars behavior
    implicit val opts: Options =
      if (isBlock || !isEscaped)
        options.copy(_hash = options._hash + ("noEscape" -> BooleanEvalResult(true)))
      else
        options.copy(_hash = options._hash + ("noEscape" -> BooleanEvalResult(false)))
    (opts, eval(opts))
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
