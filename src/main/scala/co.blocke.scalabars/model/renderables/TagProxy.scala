package co.blocke.scalabars
package model
package renderables

/**
 * When we render a list of Renderables we need to manage whitespace.  That means we need "phantom" tags representing the open/close tags
 * in a seq.  The tags themselves render to "", but they have before/after whitespace controls that affect how whitespace immediately
 * around the tag is handled, so we need proxy to stand in for the actual open/close tags.
 *
 * Note that while TagProxy is a Tag (not a BlockTag), they are proxies for the opening/closing tags of a block.  They are never
 * used in the context of a non-block tag.  This must be sorted out at render-time in Template.
 */
case class OpenTagProxy(arity: Int, wsCtlBefore: Boolean, wsCtlAfter: Boolean, aloneOnLine: Boolean) extends Tag {
  def render(options: Options): (Options, String) = (options, "")

  // Unused but required for Tag
  val expr = ParsedExpression("nada")
}

case class CloseTagProxy(arity: Int, wsCtlBefore: Boolean, wsCtlAfter: Boolean, aloneOnLine: Boolean) extends Tag {
  def render(options: Options): (Options, String) = (options, "")

  // Unused but required for Tag
  val expr = ParsedExpression("nada")
}
