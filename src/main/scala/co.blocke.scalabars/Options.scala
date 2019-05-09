package co.blocke.scalabars

import org.json4s._

case class Options(
    handlebars:  Scalabars,
    context:     Option[Context]  = None, // i.e. the stack, the global context, etc.
    helperName:  String           = "",
    blockParams: List[String]     = List.empty[String],
    params:      List[Argument]   = List.empty[Argument],
    _fn:         Option[Template] = None,
    _inverse:    Option[Template] = None,
    hash:        Map[String, Any] = Map.empty[String, Any]
) {
  def fn(): String = _fn.get.render(context.get)
  def fn(c: Context): String = _fn.get.render(c)
  def inverse(): String = rewireInverse().render(context.get)
  def inverse(c: Context): String = rewireInverse().render(c)
  def isFalsy(c: Context): Boolean = !calcCond(c.value)

  private def calcCond(v: JValue) = v match {
    case b: JBool   => b.value
    case JNothing   => false
    case a: JArray  => a.arr.nonEmpty
    case o: JObject => o.children.nonEmpty
    case _          => true
  }

  // In the case of inverse() we're likely running an 'else' statement.  Some else statements
  // can have nested expressions.  If so, we need to morph this Option's Inverse template into
  // a new Options having fn == this.inverse, and changing else into the embedded expression.
  // If this isn't an embedded else we can just ignore it--it renders to "".
  private def rewireInverse() = {
    _inverse match {
      case Some(SimpleTemplate(SimpleExpression("else", _, PathArgument(_) :: _, _) :: _, _)) =>
        val elseExp = _inverse.get.compiled.head.asInstanceOf[SimpleExpression]
        val firstArg = elseExp.args.head.asInstanceOf[PathArgument]
        val newExp = BlockExpression(firstArg.path.head, List(firstArg.path.head), elseExp.args.tail, _inverse.get.compiled.tail)
        SimpleTemplate(List(newExp), _inverse.get.options)
      case Some(_) =>
        _inverse.get
      case None => // Do-nothing Template
        NoopTemplate
    }
  }
}

/*
 Runtime:

    "this" -- ??? maybe context + assignments?
    hash -- ??? maybe compile options?
    params -- passed in
    blockParams -- custom stuff for blocks (stock Handlebars)--not MVP
 */ 