package co.blocke.scalabars
package model
package renderables

case class CloseTag(
    wsCtlBefore: Boolean,
    wsCtlAfter:  Boolean,
    arity:       Int     = 3 // value 2 is illegal (blocks don't escape), but may also be 4 (raw)
) extends Tag {

  val expr: ParsedExpression = null // never used for close tag

  // Manage ws ctl but output no content
  def render(rc: RenderControl): RenderControl = rc
}
