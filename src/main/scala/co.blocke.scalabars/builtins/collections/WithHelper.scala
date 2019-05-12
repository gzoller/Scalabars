package co.blocke.scalabars
package builtins.collections

import org.json4s._

case class WithHelper() extends Helper(List("target")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    val pre = lookup("target")
    pre.value match {
      case JNothing                           => options.inverse()
      case jo: JObject if jo.children.isEmpty => options.inverse()
      case _                                  => options.fn(pre)
    }
  }
}