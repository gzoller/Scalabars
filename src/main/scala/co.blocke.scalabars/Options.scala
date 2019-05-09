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
  def inverse(): String = _inverse.get.render(context.get)
  def inverse(c: Context): String = _inverse.get.render(c)
  def isFalsy(c: Context): Boolean = !calcCond(c.value)

  private def calcCond(v: JValue) = v match {
    case b: JBool   => b.value
    case JNothing   => false
    case a: JArray  => a.arr.nonEmpty
    case o: JObject => o.children.nonEmpty
    case _          => true
  }

}

/*
 Runtime:

    "this" -- ??? maybe context + assignments?
    hash -- ??? maybe compile options?
    params -- passed in
    blockParams -- custom stuff for blocks (stock Handlebars)--not MVP
 */ 