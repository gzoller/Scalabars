package co.blocke.scalabars
package builtins.comparison

case class OrHelper() extends Helper(List("a", "b")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    expr match {
      case _: BlockExpression if expr.args.map(a => !options.isFalsy(lookup(a))).foldLeft(false)(_ || _) =>
        options.fn()
      case _ =>
        options.inverse()
    }
}