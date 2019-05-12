package co.blocke.scalabars
package builtins.comparison

import co.blocke.scalabars.{ BlockExpression, Expression, Helper, Options, StringWrapper }

case class EqHelper() extends Helper(List("a", "b")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    expr match {
      case _: BlockExpression if resolve("a") == resolve("b") =>
        options.fn()
      case _ =>
        options.inverse()
    }
}