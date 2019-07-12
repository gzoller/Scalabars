package co.blocke.scalabars
package model

import org.apache.commons.text.StringEscapeUtils
import collection.JavaConverters._
import org.graalvm.polyglot.Value

case class Handlebars(private val scalabars: Scalabars) {
  def SafeString(s: String): SafeStringEvalResult = SafeStringEvalResult(s)
  def escapeExpression(s: String): String = StringEscapeUtils.escapeHtml4(s)

  def createFrame(data: Value): Value = {
    // Funky crap... Create the new empty map _in javascript_ and return this object,
    // which is interpreted JVM-side as a Java Map.  Add the given map data and return the created map.
    // There's probably a much better way to do this but it likely involves running on GraalVM.
    val empty = scalabars.js.eval("js", "({})")
    val keys = data.getMemberKeys.asScala.toList
    keys.foreach(k => empty.putMember(k, data.getMember(k)))
    empty
  }

  // Kinda dumb, for for compatibility in Scala...
  // $COVERAGE-OFF$Don't really care about this one... maybe don't use it.
  def createFrame(data: Map[String, EvalResult[_]]): Map[String, EvalResult[_]] = Map[String, EvalResult[_]]() ++ data
  // $COVERAGE-ON$

  def log(level: String, msg: String): Unit = level match {
    case "debug" => scalabars.logger.debug(msg)
    case "info"  => scalabars.logger.info(msg)
    case "error" => scalabars.logger.error(msg)
    case "warn"  => scalabars.logger.warn(msg)
    case _       => scalabars.logger.info(msg)
  }
}
