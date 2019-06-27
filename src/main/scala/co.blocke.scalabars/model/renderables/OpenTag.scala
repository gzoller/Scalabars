package co.blocke.scalabars
package model
package renderables

case class OpenTag(
    expr:        ParsedExpression, // "guts" of the tag (e.g. label, arguments)
    wsCtlBefore: Boolean,
    wsCtlAfter:  Boolean,
    arity:       Int              = 3 // value 2 is illegal (blocks don't escape), but may also be 4 (raw)
) extends Tag {

  // Manage ws ctl but output no content
  def render(rc: RenderControl): RenderControl = {
    val stage1 = if (wsCtlBefore) rc.flushLeading() else rc
    if (wsCtlAfter) stage1.flushTrailing() else stage1
  }
}
