package co.blocke.scalabars
package model
package renderables

import org.json4s._

case class InlinePartialTag(
    nameArg: Argument,
    body:    Block
) extends Renderable {

  /**
   * Render here does something different than normal.  It doesn't actually produce any output.  What it does produce
   * is a modified options; specifically the contents of this inline partial inserted into options.context so this partial
   * can be found by other Renderables in this scope.
   *
   * @param rc
   * @return
   */
  override def render(rc: RenderControl): RenderControl = {
    val name = (nameArg.eval(rc.opts) match {
      case s: StringEvalResult => Some(s.value)
      case c: ContextEvalResult =>
        c.value.value match {
          case s: JString => Some(s.values)
          case _          => None
        }
      case _ => None
    }).getOrElse(throw new BarsException("Inline partial's argument must evaluate to a string"))

    val t = SBTemplate(body.flatten.toList, rc.opts)
    val opt = rc.opts.copy(
      context = rc.opts.context.copy(partials = rc.opts.context.partials + (name -> t)))

    /*
    // Ok now we gotta do some crazy "stitching" to rewire the ws handling of the body block, because it's influenced by the {{> tag}} on replacement
    val stage1 = rc.reset()
    //    val stage1 = if (body.closeTag.aloneOnLine) rc.reset().clipTrailing() else rc.reset()
    val stage2 =
      if (body.openTag.wsCtlBefore)
        stage1.flushLeading()
      else stage1
    stage2.copy(opts = opt)
     */

    rc.copy(opts = opt)
  }

  override def toString: String =
    s"InlinePartialTag(${body.body.size})\n" + body.body
      .map(_.toString)
      .map(s => "    " + s)
      .mkString("\n")
}
