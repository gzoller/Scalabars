package co.blocke.scalabars
package model

import renderables._

//trait Block extends Renderable {
//  val openTag: OpenTag
//  val body: Seq[Renderable]
//  val closeTag: CloseTag
//
//  def render(rc: RenderControl): RenderControl =
//    (openTag +: body :+ closeTag).foldLeft(rc) { case (rcX, renderable) => renderable.render(rcX) }
//}

case class Block(openTag: OpenTag, body: Seq[Renderable], closeTag: CloseTag) //{ //extends Renderable {
//  def render(rc: RenderControl): RenderControl =
//    (openTag +: body :+ closeTag).foldLeft(rc) { case (rcX, renderable) => renderable.render(rcX) }
//}

//case class EmptyBlock() extends Block {
//  val openTag: OpenTag = OpenTag()
//  val body: Seq[Renderable] = Seq.empty[Renderable]
//  val closeTag: CloseTag = CloseTag()
//}
