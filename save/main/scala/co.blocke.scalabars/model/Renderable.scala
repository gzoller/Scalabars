package co.blocke.scalabars
package model

/**
 * A thing can be rendered to a String (output).  We return Options as well because it's possible render
 * can alter Options (known instance of this is Inline Partials add themselves to options.context).
 */
trait Renderable {
  val wsCtlBefore: Boolean
  val wsCtlAfter: Boolean

  def render(rc: RenderControl): RenderControl
  def isBlock: Boolean = false
}

//trait BlockRenderable extends Renderable {
//  val contents: Block
//  override def isBlock: Boolean = true
//}