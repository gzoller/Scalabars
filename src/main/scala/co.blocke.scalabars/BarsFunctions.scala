package co.blocke.scalabars

import org.apache.commons.text.StringEscapeUtils
import javax.script.ScriptEngine

case class BarsFunctions(js: ScriptEngine) {

  def escapeExpression(s: String): String = StringEscapeUtils.escapeHtml4(s)
  def SafeString(s: String) = {
    js.eval("this.safestring = true")
    s
  }

  // Unsupported:
  //-------------------------------
  // Handlebars.precompile(template, options)
  // Handlebars.template(templateSpec)
  // Handlebars.createFrame(data)
  // Handlebars.create()
  // Handlebars.noConflict()
  // Handlebars.log(level, message)

  js.put("Handlebars", this)

}
