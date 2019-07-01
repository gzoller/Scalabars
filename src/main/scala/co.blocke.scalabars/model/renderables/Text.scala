package co.blocke.scalabars
package model
package renderables

case class Text(s: String) extends Renderable {
  def render(rc: RenderControl): RenderControl = rc.addText(s)
}
