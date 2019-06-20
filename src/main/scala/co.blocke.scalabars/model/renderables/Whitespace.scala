package co.blocke.scalabars
package model
package renderables

// Space, Tab, NL
case class Whitespace(ws: String, isLast: Boolean = false) extends Renderable {
  val wsCtlBefore: Boolean = false
  val wsCtlAfter: Boolean = false

  def render(rc: RenderControl): RenderControl = rc.addWS(ws)

  def setLast(last: Boolean): Renderable = this.copy(isLast = last)
}