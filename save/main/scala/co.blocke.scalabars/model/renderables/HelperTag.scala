package co.blocke.scalabars
package model
package renderables

case class HelperTag(
    nameOrPath:  String,
    helper:      Helper,
    arity:       Int,
    wsCtlBefore: Boolean,
    wsCtlAfter:  Boolean,
    wsAfter:     String,
    expr:        ParsedExpression,

    // If block helper
    isInverted:  Boolean     = false,
    blockParams: Seq[String] = Seq.empty[String],
    body:        Block       = EmptyBlock()
) extends BlockTag with Evalable {

  override def isBlock: Boolean = !body.isInstanceOf[EmptyBlock]

  /*
  override def render(rc: RenderControl): RenderControl = {
    helper match {
      case ph: PartialHelper =>
        val contextualize = isBlock match {
          case true =>
            ???
          //            contents match {
          //              case openTag +: body :+ closeTag =>
          //                val otag = openTag.asInstanceOf[OpenTagProxy]
          //                val ctag = closeTag.asInstanceOf[CloseTagProxy]
          //                (in: String) =>
          //                  (otag +: sliceToText(in) :+ ctag).foldLeft(rc) { case (rcX, renderable) => renderable.render(rcX) }.out.toString
          //            }
          case false =>
            (in: String) =>
              (OpenTagProxy(2, wsCtlBefore, false, "") +: sliceToText(in) :+ CloseTagProxy(2, false, wsCtlAfter, wsAfter))
                .foldLeft(rc) { case (rcX, renderable) => renderable.render(rcX) }.out.toString
        }
        val stage0 = rc.copy(opts = rc.opts.copy(contextualize = contextualize))
        val stage1 = checkClipAndFlush(stage0.reset(), this)
        implicit val escapeOpts: Options = stage1.opts
        stage1.addContent(eval(stage1.opts))

      // Non-Partial block helper
      case _ if isBlock =>
        contents match {
          case openTag +: body :+ closeTag =>
            val otag = openTag.asInstanceOf[OpenTagProxy]
            val ctag = closeTag.asInstanceOf[CloseTagProxy]
            val fn = (in: String) =>
              (otag +: sliceToText(in) :+ ctag).foldLeft(RenderControl(rc.opts)) { case (rc, renderable) => renderable.render(rc) }.out.toString
            rc.copy(opts = rc.opts.copy(contextualize = fn))
            super.render(rc)
        }

      // Simple, non-block, non-Partial helper tag
      case _ =>
        implicit val escapeOpts: Options = rc.opts.copy(_hash = rc.opts._hash + ("noEscape" -> BooleanEvalResult(!isEscaped)))
        val stage1 = checkFlush(rc.reset(), this)
        stage1.addContent(eval(stage1.opts))
    }
  }
   */

  /*
//    val contextualizeWS = (a:String) => {
//      checkClipAndFlush(rc, this)
//    }
    if (isBlock) {
      super.render(rc)
    } else helper match {
      case ph: PartialHelper =>
        // Special handling for PartialHelper, which replaces a non-block tag with a block tag.  Need to mock up a block
        // tag and render.
        // TODO: Non-inline partials need open/close block wrappers w/ws to work!
        // TODO: Can we detect inline partial here and sew tag proxies here?  More to the point--can we isolate the
        //    content creation apart from ws handling?
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
  }
*/

  def eval(options: Options): EvalResult[_] =
    if (nameOrPath == "@partial-block")
      options.context.lookup(nameOrPath).toEvalResult(options)
    else
      helper.run()(bakeOptions(options), Map.empty[String, Template])

  // Returns (fn, inverse) templates
  private def examineBlock(options: Options): (Block, Block) =
    if (isBlock)
      (body, EmptyBlock())
    else
      (EmptyBlock(), EmptyBlock())
  /*
  private def examineBlock(options: Options): (Template, Template) =
    if (body.isEmpty)
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
   */

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

  private def sliceToText(s: String): Seq[Renderable] = {
    val (wsBefore, rest) = s.indexWhere(c => !c.isWhitespace) match {
      case -1 => (Whitespace(""), s)
      case i  => (Whitespace(s.take(i)), s.drop(i))
    }
    val (body, wsAfter) = rest.lastIndexWhere(c => c.isWhitespace) match {
      case -1 => (rest, Whitespace(""))
      case i  => (rest.dropRight(i), Whitespace(rest.takeRight(i - 1)))
    }
    Seq(wsBefore, Text(body), wsAfter)
  }

  override def toString(): String = {
    s"HelperTag $nameOrPath ($helper)\n" +
      "  args: " + expr.args + "\n" +
      "  contents: \n" +
      body.map(_.toString).map(s => "    " + s).mkString("\n")
  }
}
