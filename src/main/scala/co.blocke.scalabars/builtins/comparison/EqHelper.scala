package co.blocke.scalabars
package builtins.comparison

case class EqHelper() extends Helper(List("a", "b")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    expr match {
      case _: BlockExpression if resolve("a") == resolve("b") =>
        options.fn()
      case _ =>
        options.inverse()
    }
}
