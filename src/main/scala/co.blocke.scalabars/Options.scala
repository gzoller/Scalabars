package co.blocke.scalabars

import org.json4s._
import collection.JavaConverters._

case class Options(
    handlebars:  Scalabars,
    context:     Option[Context]       = None, // i.e. the stack, the global context, etc.
    helperName:  String                = "",
    blockParams: List[String]          = List.empty[String],
    params:      List[Argument]        = List.empty[Argument],
    _fn:         Option[Template]      = None,
    _inverse:    Option[Template]      = None,
    _hash:       Map[String, Argument] = Map.empty[String, Argument]
) {
  def fn(): String = _fn.get.render(context.get)
  def fn(c: Context): String = _fn.get.render(c)

  // This one is called from JavaScript -- TODO: may be a different input type when migrating off Nashorn!
  def fn(m: scala.collection.convert.Wrappers.MapWrapper[String, Any]): String = {
    val scalaM = m.keySet.asScala.zip(m.values.asScala).toMap
    fn(Context(handlebars.sjJson.render(scalaM)))
  }

  def inverse(): String = rewireInverse().render(context.get)
  // $COVERAGE-OFF$Don't really need/use this... included for completeness.  Not sure how to test!
  def inverse(c: Context): String = rewireInverse().render(c)
  // $COVERAGE-ON$
  def isFalsy(c: Context): Boolean = !calcCond(c.value)
  def hash(key: String): String = {
    val (root :: rest) = handlebars.parsePath(key)
    _hash.get(root) match {
      case Some(s: StringArgument) => s.value
      case Some(a: PathArgument)   => context.get.resolve(a.path ++ rest, this)
      case _                       => ""
    }
  }

  private def calcCond(v: JValue) = v match {
    case b: JBool         => b.value
    case JNothing         => false
    case a: JArray        => a.arr.nonEmpty
    case o: JObject       => o.children.nonEmpty
    case JString("false") => false
    case _                => true
  }

  // In the case of inverse() we're likely running an 'else' statement.  Some else statements
  // can have nested expressions.  If so, we need to morph this Option's Inverse template into
  // a new Options having fn == this.inverse, and changing else into the embedded expression.
  // If this isn't an embedded else we can just ignore it--it renders to "".
  private def rewireInverse() =
    _inverse match {
      case Some(SimpleTemplate(SimpleExpression("else", _, PathArgument(_) :: _, _) :: _, _)) =>
        val elseExp = _inverse.get.compiled.head.asInstanceOf[SimpleExpression]
        val firstArg = elseExp.args.head.asInstanceOf[PathArgument]
        val newExp = BlockExpression(firstArg.path.head, List(firstArg.path.head), elseExp.args.tail, _inverse.get.compiled.tail)
        SimpleTemplate(List(newExp), _inverse.get.options)
      case Some(x) => x
      // $COVERAGE-OFF$Should Never Happen(tm)
      case None    => throw new BarsException("rewiring inverse failed.  Should Never Happen(tm)")
      // $COVERAGE-ON$
    }
}
