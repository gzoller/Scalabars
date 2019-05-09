package co.blocke.scalabars
package builtins

case class IfHelper() extends Helper(List("conditional")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    if (!options.isFalsy(lookup("conditional")))
      options.fn()
    else
      options.inverse()
}
