package co.blocke.scalabars
package builtins.stock

import co.blocke.scalabars.{ Expression, Helper, Options, StringWrapper }

case class UnlessHelper() extends Helper(List("conditional")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    if (options.isFalsy(lookup("conditional")))
      options.fn()
    else
      options.inverse()
}