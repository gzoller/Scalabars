package co.blocke.scalabars
package model

import org.json4s._

trait EvalResult[T] {
  val value: T

  /**
    * Subscript that is context-aware (arrays/objects).  Only ContextEvalResults can really implement this,
    * but included in all of them so we don't need a bunch of special handlers.
    * @param idx
    * @param options
    * @return None if idx type doesn't match this type, e.g. non-Int for an Array
    */
  // $COVERAGE-OFF$Never called exception for override.
  def >>(idx: EvalResult[_])(implicit options: Options): Option[Context] = None
  // $COVERAGE-ON$

  override def toString(): String = value.toString
}

case class StringEvalResult(value: String) extends EvalResult[String]

/**
  * Like StringEvalResult, except that the wrapped string will not be escaped.
  */
case class SafeStringEvalResult(value: String) extends EvalResult[String]

case class ContextEvalResult(value: Context) extends EvalResult[Context] {

  def isArray: Boolean = value.value match {
    case _: JArray => true
    case _         => false
  }

  def isObject: Boolean = value.value match {
    case _: JObject => true
    case _          => false
  }

  override def toString(): String = {
    val s: String = value.value
    s
  }

  override def >>(idx: EvalResult[_])(implicit options: Options): Option[Context] = {
    value.value match {
      case a: JArray if a.arr.nonEmpty =>
        (idx.value match {
          case i: Long                          => Some(i.toInt)
          case Context(_, JLong(i), _, _, _, _) => Some(i.toInt)
          case Context(_, JInt(i), _, _, _, _)  => Some(i)
          case _                                => None
        }).map(i => value.lookup(prefixPath(value.path, s"[$i]"))).orElse {
          // Could be further drill-down: [2].foo (String idx, not Int)
          (idx.value match {
            case s: String                          => Some(s)
            case Context(_, JString(s), _, _, _, _) => Some(s)
            case _                                  => None
          }).map(s => value.lookup(prefixPath(value.path, s)))
        }
      case a: JObject if a.children.nonEmpty =>
        (idx.value match {
          case s: String                          => Some(s)
          case Context(_, JString(s), _, _, _, _) => Some(s)
          case _                                  => None
        }).map(s => value.lookup(prefixPath(value.path, s)))
      case _ => None
    }
  }
}

case class BooleanEvalResult(value: Boolean) extends EvalResult[Boolean]

case class LongEvalResult(value: Long) extends EvalResult[Long]

case class DoubleEvalResult(value: Double) extends EvalResult[Double]

// A bit of a cheat here.  Alloss a Helper to "become" something else, essentially replacing its parent.
// The mechanism is the Helper would return the new Renderable and the parent would render this instead of
// itself.  Intended foremost for ParitalHelper.
case class RenderableEvalResult(value: Renderable) extends EvalResult[Renderable]

case class NoEvalResult() extends EvalResult[String] {
  val value = ""
}

// And some unapply magic...
object AsArray {
  def unapply(er: EvalResult[_]): Option[JArray] = er match {
    case c: ContextEvalResult if c.isArray => Some(c.value.value.asInstanceOf[JArray])
    case _                                 => None
  }
}
object AsObject {
  def unapply(er: EvalResult[_]): Option[JObject] = er match {
    case c: ContextEvalResult if c.isObject => Some(c.value.value.asInstanceOf[JObject])
    case _                                  => None
  }
}
