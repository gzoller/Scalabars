package co.blocke.scalabars
package model

trait Tag extends Renderable {
  val arity: Int
  val wsCtlBefore: Boolean
  val wsCtlAfter: Boolean
  val expr: ParsedExpression
  val aloneOnLine: Boolean

  def isEscaped: Boolean = arity == 2
}

trait BlockTag extends Tag with BlockRenderable {
  val blockParams: Seq[String]
}
