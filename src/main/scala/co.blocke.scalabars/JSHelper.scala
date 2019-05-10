package co.blocke.scalabars

import org.json4s.native.JsonMethods

case class JSHelper(name: String, js: String) extends Helper() {

  private var hasCompiledJS = false

  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    if (!hasCompiledJS) {
      options.handlebars.run(s"var $name = " + js)
      hasCompiledJS = true
    }
    val contextVars = JsonMethods.compact(JsonMethods.render(options.context.get.value))
    options.handlebars.runHelper(name, options.context.get, options.params.map(a => stringifyValue(lookup(a).value)) :+ options)
  }
}
