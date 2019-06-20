package co.blocke.scalabars
package model
package renderables

trait HelperCommon {
  // Do everything necessary to update Options before eval() on this helper.  Set up any current state
  // held in Options or to be passed down by parent.
  def bakeOptions(name: String, expr: ParsedExpression, options: Options): Options = {
    val (assignments, literalsAndPaths) = expr.args.partition(_.isInstanceOf[AssignmentArgument]).asInstanceOf[(Seq[AssignmentArgument], Seq[Argument])]

    // Poke assignments into Options.hash
    val hashAdds: Seq[(String, EvalResult[_])] = assignments.map(a => (a.label, a.targetValue.eval(options)))

    // Now eval all literals and paths and put EvalResults into params
    val evaledParams = literalsAndPaths.map(_.eval(options)).toList

    // Cook the newly populated Options
    options.copy(
      helperName  = name,
      params      = evaledParams,
      paramValues = literalsAndPaths.map(_.value).toList,
      _hash       = options._hash ++ hashAdds.toMap)
  }

  // Eval comes back with a String.  This converts the String into:  Whitespace, Text, Whitespace
  def sliceToRenderables(s: String): Seq[Renderable] = {
    val (wsBefore, rest) = s.indexWhere(c => !c.isWhitespace) match {
      case -1 => (Whitespace(""), s)
      case i  => (Whitespace(s.take(i)), s.drop(i))
    }
    val (body, wsAfter) = rest.lastIndexWhere(c => !c.isWhitespace) match {
      case -1 => (rest, Whitespace(""))
      case i  => (rest.take(i + 1), Whitespace(rest.drop(i + 1)))
    }
    Seq(wsBefore, Text(body), wsAfter)
  }
}

case class HelperTag(
    nameOrPath:  String,
    helper:      Helper,
    expr:        ParsedExpression, // "guts" of the tag (e.g. label, arguments)
    wsCtlBefore: Boolean,
    wsCtlAfter:  Boolean,
    wsAfter:     String, // used to determine if tag is alone on line
    arity:       Int,
    isLast:      Boolean          = false
) extends Tag with Evalable with HelperCommon {

  def render(rc: RenderControl): RenderControl = {
    implicit val escapeOpts: Options = rc.opts.copy(_hash = rc.opts._hash + ("noEscape" -> BooleanEvalResult(!isEscaped)))
    val body = sliceToRenderables(eval(escapeOpts))

    val stage1 = if (wsCtlBefore) rc.flushLeading() else rc
    val stage2 = body.foldLeft(stage1) { case (rcX, renderable) => renderable.render(rcX) }
    if (wsCtlAfter) stage2.flushTrailing() else stage2
  }

  def eval(options: Options): EvalResult[_] =
    if (nameOrPath == "@partial-block")
      options.context.lookup(nameOrPath).toEvalResult(options)
    else
      helper.run()(bakeOptions(nameOrPath, expr, options), Map.empty[String, Template])

  def setLast(last: Boolean): Renderable = this.copy(isLast = last)

  override def toString(): String = {
    s"HelperTag $nameOrPath ($helper)\n" +
      "  args: " + expr.args + "\n"
  }
}
