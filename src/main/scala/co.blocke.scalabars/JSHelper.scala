package co.blocke.scalabars

case class JSHelper(name: String, js: String) extends Helper() {

  private var hasCompiledJS = false

  def run(expr: Expression)(implicit options: Options): StringWrapper = {
    if (!hasCompiledJS) {
      options.handlebars.run(s"var $name = " + js)
      hasCompiledJS = true
    }
    options.handlebars.runHelper(name, options.context.get, options.params.map(a => packValue4js(lookup(a).value)) :+ options)
  }
}
