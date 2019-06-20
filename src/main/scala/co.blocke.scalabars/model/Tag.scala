package co.blocke.scalabars
package model

import renderables._

trait Tag extends Renderable {
  val arity: Int
  val wsCtlBefore: Boolean
  val wsCtlAfter: Boolean
  val wsAfter: String // used to determine if tag is alone on line
  val expr: ParsedExpression // "guts" of the tag (e.g. label, arguments)

  def isEscaped: Boolean = arity == 2

}

/*
trait BlockTag extends Tag with Renderable {
  val body: Block
  val blockParams: Seq[String]

  // For non-block tags, we don't check clipping, just flushing (ws ctl ~).
  protected def checkFlush(rcIn: RenderControl, tag: Tag): RenderControl = {
    //    println(rcIn.ws())
    val stage1 = if (tag.wsCtlBefore) rcIn.flushLeading() else rcIn
    //    println(stage1.ws())
    if (tag.wsCtlAfter) stage1.flushTrailing() else stage1
  }

  def render(rc: RenderControl): RenderControl = {
    val stage1 = if (body.openTag.wsCtlBefore) rc.clipLeading() else rc
    val stage2 = if (body.openTag.wsCtlAfter) stage1.clipTrailing() else stage1
    val stage3 = body.body.foldLeft(stage2) { case (rcX, renderable) => renderable.render(rcX) }
    val stage4 = if (body.closeTag.wsCtlBefore) stage3.clipLeading() else stage3
    val stage5 = if (body.closeTag.wsCtlAfter) stage4.clipLeading() else stage4
    stage5
  }

  /*
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
   */
}

 */
