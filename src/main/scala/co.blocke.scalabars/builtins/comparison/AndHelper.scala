package co.blocke.scalabars
package builtins.comparison

case class AndHelper() extends Helper() { // get params from expr since it's varargs
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    expr match {
      case _: BlockExpression if expr.args.map(a => !options.isFalsy(lookup(a))).forall(_ == true) =>
        options.fn()
      case _ =>
        options.inverse()
    }
}
