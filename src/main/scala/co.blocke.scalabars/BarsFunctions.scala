package co.blocke.scalabars

import org.apache.commons.text.StringEscapeUtils
import javax.script.ScriptEngine

case class BarsFunctions(js: ScriptEngine) {

  def escapeExpression(s: String): String = StringEscapeUtils.escapeHtml4(s)
  def SafeString(s: String) = {
    js.eval("this.safestring = true")
    s
  }

  js.eval("""function merge(obj1, obj2) {
            |  for (var p in obj2) {
            |    try {
            |      // Property in destination object set; update its value.
            |      if ( obj2[p].constructor==Object ) {
            |        obj1[p] = MergeRecursive(obj1[p], obj2[p]);
            |      } else {
            |        obj1[p] = obj2[p];
            |      }
            |    } catch(e) {
            |      // Property in destination object not set; create it and set its value.
            |      obj1[p] = obj2[p];
            |    }
            |  }
            |  return obj1;
            |}""".stripMargin)

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
