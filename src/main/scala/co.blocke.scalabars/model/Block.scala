package co.blocke.scalabars
package model

import renderables._

object Block {
  def apply(t: Template): Option[Block] = t.compiled match {
    case List(ot: OpenTag, _*) =>
      val ot = t.compiled.head.asInstanceOf[OpenTag]
      val stage1 = t.compiled.tail.reverse
      stage1 match {
        case List(ct: CloseTag, _*) =>
          val ct = stage1.head.asInstanceOf[CloseTag]
          Some(Block(ot, stage1.tail.reverse, ct))
        case _ => None
      }
    case _ => None
  }
}

case class Block(openTag: OpenTag, body: Seq[Renderable], closeTag: CloseTag) {
  def flatten: Seq[Renderable] = openTag +: body :+ closeTag
  def toTemplate(opts: Options) = SBTemplate(flatten.toList, opts)
}

//trait Block extends Renderable {
//  val openTag: OpenTag
//  val body: Seq[Renderable]
//  val closeTag: CloseTag
//
//  def render(rc: RenderControl): RenderControl =
//    (openTag +: body :+ closeTag).foldLeft(rc) { case (rcX, renderable) => renderable.render(rcX) }
//}

//{ //extends Renderable {
//  def render(rc: RenderControl): RenderControl =
//    (openTag +: body :+ closeTag).foldLeft(rc) { case (rcX, renderable) => renderable.render(rcX) }
//}

//case class EmptyBlock() extends Block {
//  val openTag: OpenTag = OpenTag()
//  val body: Seq[Renderable] = Seq.empty[Renderable]
//  val closeTag: CloseTag = CloseTag()
//}
