package co.blocke.scalabars
package model
package renderables

// Space, Tab, NL
// wasLinePruned can be set true by PostParse to indicate whether this ws had previously contained a \n.
// (needed to determine if following {{tag}} is alone on its line)
case class Whitespace(ws: String, clippedWS: String = "", isClipped: Boolean = false) extends Renderable {
  def render(rc: RenderControl): RenderControl = rc.addWS(this)

  override def toString: String = {
    val s =
      s"""Whitespace |${ws.replaceAll("\n", "\\\\n")}|"""
    if (isClipped) s + s""" clipped: |$clippedWS|"""
    else
      s
  }
}
