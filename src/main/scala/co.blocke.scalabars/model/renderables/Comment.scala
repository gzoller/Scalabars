package co.blocke.scalabars
package model
package renderables

// Basically a zero-length Text field (prints nothing but affects rendering as a Text element would)
case class Comment(s: String) extends Renderable {
  def render(rc: RenderControl): RenderControl = rc
}
