package co.blocke.scalabars
package model

import renderables._

trait Tag extends Renderable {
  val arity: Int
  val wsCtlBefore: Boolean
  val wsCtlAfter: Boolean
  val wsAfter: String // used to determine if tag is alone on line
  val expr: ParsedExpression

  def isEscaped: Boolean = arity == 2

  // Utility to see if a tag exists on the same line (only whitespace)
  protected def aloneOnLine(rc: RenderControl, before: String, after: String): Boolean = {
    val clearBefore = before.reverse.indexWhere(c => c == '\n' || !c.isWhitespace) match {
      case -1                     => true
      case i if before(i) == '\n' => true
      case _                      => false
    }
    val clearAfter = after.indexWhere(c => c == '\n' || !c.isWhitespace) match {
      case -1                    => true
      case i if after(i) == '\n' => true
      case _                     => false
    }
    (rc.isFirst && clearAfter) || (clearBefore && clearAfter)
  }

  // For non-block tags, we don't check clipping, just flushing (ws ctl ~).
  protected def checkFlush(rcIn: RenderControl, tag: Tag): RenderControl = {
    //    println(rcIn.ws())
    val stage1 = if (tag.wsCtlBefore) rcIn.flushLeading() else rcIn
    //    println(stage1.ws())
    if (tag.wsCtlAfter) stage1.flushTrailing() else stage1
  }
}

trait BlockTag extends Tag with BlockRenderable {
  val contents: Seq[Renderable]
  val blockParams: Seq[String]

  protected def checkClipAndFlush(rcIn: RenderControl, tag: Tag): RenderControl = {
    //    println(rcIn.ws())
    val stage1 = if (aloneOnLine(rcIn, rcIn.accumulatedWS, tag.wsAfter)) rcIn.clipLeading().clipTrailing() else rcIn
    //    println(stage1.ws())
    val stage2 = if (tag.wsCtlBefore) stage1.flushLeading() else stage1
    //    println(stage2.ws())
    if (tag.wsCtlAfter) stage2.flushTrailing() else stage2
  }

  override def render(rc: RenderControl): RenderControl = {
    val openTag = contents.head.asInstanceOf[OpenTagProxy]
    val closeTag = contents.last.asInstanceOf[CloseTagProxy]

    val stage3 = checkClipAndFlush(rc.reset(), openTag)

    val subcontents = contents.tail
    val stage4 = subcontents.take(subcontents.length - 1).foldLeft(stage3) { case (rcx, renderable) => renderable.render(rcx) }

    checkClipAndFlush(stage4, closeTag)
  }
}
