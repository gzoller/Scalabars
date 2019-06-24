package co.blocke.scalabars
package model

import org.apache.commons.text.StringEscapeUtils
import collection.JavaConverters._
import org.graalvm.polyglot.Value

// TODO:  Likely want 2 Handlebars objects...
// TODO:    One for Scala, which isn't really needed but will make native helper authors feel at home
// TODO:    The second one in pure Javascript so we can do things like: new SafeString()
// TODO:      --> Maybe there's a hidden Scala handlebars thing that JS Handlebars call as a service to create SafeStringWrapper?

case class Handlebars(private val scalabars: Scalabars) {
  def SafeString(s: String) = SafeStringEvalResult(s)
  def escapeExpression(s: String): String = StringEscapeUtils.escapeHtml4(s)

  def createFrame(data: Value): Value = {
    // Funky crap... Create the new empty map _in javascript_ and return this object,
    // which is interpreted JVM-side as a Java Map.  Add the given map data and return the created map.
    // There's probably a much better way to do this but it likely involves running on GraalVM.
    val empty = scalabars.js.eval("js", "({})")
    val keys = data.getMemberKeys.asScala.toList
    keys.map(k => empty.putMember(k, data.getMember(k)))
    empty
  }

  // Kinda dumb, for for compatibility in Scala...
  def createFrame(data: Map[String, EvalResult[_]]): Map[String, EvalResult[_]] = Map[String, EvalResult[_]]() ++ data
}