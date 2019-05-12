package co.blocke.scalabars
package builtins.collections

import org.json4s._

case class EachPropertyHelper() extends Helper(List("obj")) {
  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    val itemsContext = lookup("obj")
    val obj = itemsContext.value.asInstanceOf[JObject].values
    if (obj.nonEmpty)
      obj.keySet.map { key =>
        val valueCtx = lookup(s"obj.$key")
        options.fn(valueCtx.copy(extras = valueCtx.extras ++ Map("key" -> Context(JString(key)), "value" -> valueCtx)))
      }.mkString
    else
      options.inverse()
  }
}