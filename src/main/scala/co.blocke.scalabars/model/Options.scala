package co.blocke.scalabars
package model

import org.json4s._

case class Options(
    handlebars:  Scalabars,
    context:     Context                    = Context.NotFound,
    helperName:  String                     = "",
    blockParams: List[String]               = List.empty[String],
    params:      List[EvalResult[_]]        = List.empty[EvalResult[_]],
    paramValues: List[String]               = List.empty[String],
    _fn:         Template                   = EmptyTemplate(),
    _inverse:    Template                   = EmptyTemplate(),
    _hash:       Map[String, EvalResult[_]] = Map.empty[String, EvalResult[_]]
) {

  // _hash() returns EvalResult for an intended future purpose so that you can sub-path off the result,
  // i.e.:  hash("foo")/bar[2].  This actually works in JS, but would (currently) be ungainly in Scala without
  // a DSL to help Helper authors.  For now we cheat and crash-resolve the EvalResult directly to String,
  // which means hash values used in Scala Helpers should resolve to scalars, not contexts.
  def hash(key: String): String = er2string(_hash.getOrElse(key, NoEvalResult()))(this)

  def data: Map[String, EvalResult[_]] = context.data
  def asJava = new JavaOptions(this)

  def fn(): String = fn(context, Map.empty[String, EvalResult[_]], Map.empty[String, Context])
  def fn(c: Context): String = fn(c, Map.empty[String, EvalResult[_]], Map.empty[String, Context])
  def fn(c: Context, data: Map[String, EvalResult[_]]): String =
    fn(c, data, Map.empty[String, Context])
  def fn(
      c:           Context,
      data:        Map[String, EvalResult[_]],
      blockParams: Map[String, Context]
  ): String = {
    _fn.render(c.copy(data        = data, blockParams = c.blockParams ++ blockParams))
  }

  def inverse(): String = _inverse.render(context)
  // $COVERAGE-OFF$Don't really need/use this... included for completeness.  Not sure how to test!
  def inverse(c: Context): String = _inverse.render(c)
  // $COVERAGE-ON$

  def isFalsy(er: EvalResult[_]): Boolean = er match {
    case c: ContextEvalResult =>
      c.value.value match {
        case b: JBool   => !b.value
        case JNothing   => true
        case a: JArray  => a.arr.isEmpty
        case o: JObject => o.children.isEmpty
        case _          => false
      }
    case StringEvalResult("false") => true
    case BooleanEvalResult(false)  => true
    case NoEvalResult()            => true
    case _                         => false
  }

  override def toString = "Options:\n   Hash = " + _hash + "\n   Context = " + context
}
