package co.blocke.scalabars
package model
package renderables

case class Text(s: String, isLast: Boolean = false) extends Renderable {
  val wsCtlBefore: Boolean = false
  val wsCtlAfter: Boolean = false

  def render(rc: RenderControl): RenderControl = rc.addText(s)

  def setLast(last: Boolean): Renderable = this.copy(isLast = last)
}