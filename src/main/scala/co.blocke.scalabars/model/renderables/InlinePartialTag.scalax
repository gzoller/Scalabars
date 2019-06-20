package co.blocke.scalabars
package model
package renderables

import org.json4s._

case class InlinePartialTag(
    nameArg:     Argument,
    contents:    Seq[Renderable],
    wsCtlBefore: Boolean,
    wsCtlAfter:  Boolean,
    wsAfter:     String
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
   * @param rc
   * @return
   */
  override def render(rc: RenderControl): RenderControl = {
    val name = (nameArg.eval(rc.opts) match {
      case s: StringEvalResult => Some(s.value)
      case c: ContextEvalResult => c.value.value match {
        case s: JString => Some(s.values)
        case _          => None
      }
      case _ => None
    }).getOrElse(throw new BarsException("Inline partial's argument must evaluate to a string"))

    val t = SBTemplate(contents.toList, rc.opts)
    val opt = rc.opts.copy(context = rc.opts.context.copy(partials = rc.opts.context.partials + (name -> t)))

    val openTag = contents.head.asInstanceOf[OpenTagProxy]
    val closeTag = contents.last.asInstanceOf[CloseTagProxy]
    val stage1 = checkClipAndFlush(rc.reset(), openTag)
    val stage2 = checkClipAndFlush(stage1, closeTag)
    stage2.copy(opts = opt) // no change in content output
  }

  override def toString: String = s"InlinePartialTag(${contents.size})\n" + contents.map(_.toString).map(s => "    " + s).mkString("\n")
}