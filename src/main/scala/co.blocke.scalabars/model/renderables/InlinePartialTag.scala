package co.blocke.scalabars
package model
package renderables

import org.json4s._

case class InlinePartialTag(
    nameArg:     Argument,
    contents:    Seq[Renderable],
    wsCtlBefore: Boolean,
    wsCtlAfter:  Boolean,
    aloneOnLine: Boolean
) extends BlockTag {
  val arity: Int = 0
  val expr: ParsedExpression = null // CAREFUL!
  val blockParams: Seq[String] = Nil

  override def isBlock: Boolean = true

  /**
   * Render here does something different than normal.  It doesn't actually produce any output.  What it does produce
   * is a modified options; specifically the contents of this inline partial inserted into options.context so this partial
   * can be found by other Renderables in this scope.
   *
   * @param options
   * @return
   */
  def render(options: Options): (Options, String) = {
    val name = (nameArg.eval(options) match {
      case s: StringEvalResult => Some(s.value)
      case c: ContextEvalResult => c.value.value match {
        case s: JString => Some(s.values)
        case _          => None
      }
      case _ => None
    }).getOrElse(throw new BarsException("Inline partial's argument must evaluate to a string"))

    val t = SBTemplate(contents.toList, options)
    val opt = options.copy(context = options.context.copy(partials = options.context.partials + (name -> t)))
    (opt, "")
  }

  override def toString: String = s"InlinePartialTag(${contents.size})\n" + contents.map(_.toString).map(s => "    " + s).mkString("\n")
}