package co.blocke.scalabars
package model

import renderables._

/*
trait Block extends Renderable {

  val openTag: OpenTagProxy
  val body: Seq[Renderable]
  val closeTag: CloseTagProxy

  def render(rc: RenderControl): RenderControl = {
    val stage1 = if(openTag.wsCtlBefore) rc.clipLeading() else rc
    val stage2 = if(openTag.wsCtlAfter) stage1.clipTrailing() else stage1
    val stage3 = body.foldLeft(stage2) { case (rcX, renderable) => renderable.render(rcX) }
    val stage4 = if(closeTag.wsCtlBefore) stage3.clipLeading() else stage3
    val stage5 = if(closeTag.wsCtlAfter) stage4.clipLeading() else stage4
    stage5
  }
}
 */

trait Block {
  val openTag: OpenTagProxy
  val body: Seq[Renderable]
  val closeTag: CloseTagProxy
}

case class BlockBody(openTag: OpenTagProxy, body: Seq[Renderable], closeTag: CloseTagProxy) extends Block
case class EmptyBlock() extends Block {
  val openTag: OpenTagProxy = OpenTagProxy()
  val body: Seq[Renderable] = Seq.empty[Renderable]
  val closeTag: CloseTagProxy = CloseTagProxy()
}

//
//  // Utility to see if a tag exists on the same line (only whitespace)
//  protected def aloneOnLine(rc: RenderControl, tag: Tag): Boolean = {
//    val clearBefore = rc.accumulatedWS.contains("\n") || rc.isFirst
//    val clearAfter = tag.wsAfter.contains("\n")
//    clearBefore && clearAfter
//  }

