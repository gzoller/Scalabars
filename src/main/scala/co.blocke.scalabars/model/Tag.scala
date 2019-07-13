package co.blocke.scalabars
package model

import renderables._

trait Tag extends Renderable {
  val arity: Int
  val wsCtlBefore: Boolean
  val wsCtlAfter: Boolean
  val expr: ParsedExpression // "guts" of the tag (e.g. label, arguments)

  def isEscaped: Boolean = arity == 2

}
