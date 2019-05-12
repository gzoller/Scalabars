package co.blocke.scalabars
package builtins.misc

import org.json4s._

case class DefaultHelper() extends Helper(List("lookup", "fallback")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper =
    lookup("lookup").value match {
      case JNothing | JNull => resolve("fallback")
      case _                => resolve("lookup")
    }
}
