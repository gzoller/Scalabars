package co.blocke.scalabars
package helpers.misc

import model._

case class IncludeHelper() extends Helper("filePath", "applyContext") {
  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    val fileStr = options.handlebars.fileGetter.retrieveFileAtPath(arg("filePath"))
    implicit val ctx: Context = options.context // trigger implicit EvalResult->Context conversion
    options.handlebars.compile(fileStr).render(arg("applyContext"))
  }
}
