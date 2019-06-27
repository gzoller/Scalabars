package co.blocke.scalabars
package model
package renderables

case class Text(s: String) extends Renderable {
  val wsCtlBefore: Boolean = false
  val wsCtlAfter: Boolean = false

  def render(rc: RenderControl): RenderControl = rc.addText(s)
}
