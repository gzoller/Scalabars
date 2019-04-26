package co.blocke.scalabars

import reflect.runtime.universe.TypeTag
import javax.script.{ ScriptEngine, ScriptEngineManager }

case class ScalaBars(
    template: String,
    partials: Map[String, String] = Map.empty[String, String],
    helpers:  Map[String, String] = Map.empty[String, String]) {

  private val parser = HandlebarsParser()
  private val parsed = parser.compile(template)
  private var compiledPartials = Map.empty[String, List[Renderable]]
  private var hasEvaledHelpers = false
  private val javascript = new ScriptEngineManager().getEngineByName("nashorn")

  def registerPartial(name: String, template: String): ScalaBars = this.copy(partials = this.partials + (name -> template))
  def registerHelper(name: String, code: String): ScalaBars = this.copy(helpers = this.helpers + (name -> code))

  def render[T](context: T)(implicit tt: TypeTag[T]): String = render(SB.sj.render(context))
  def render(context: SB.Scope): String = {
    if (compiledPartials.isEmpty && partials.nonEmpty)
      compiledPartials = partials.map { case (k, v) => (k, parser.compile(v)) }
    if (!hasEvaledHelpers && helpers.nonEmpty)
      helpers.map { case (k, v) => javascript.eval(s"var $k = " + v) }
    parsed.map(_.render(Context(context))(this)).mkString("")
  }

  private[scalabars] def getPartial(name: String): Option[List[Renderable]] = compiledPartials.get(name)
  private[scalabars] def run(fnName: String): String = javascript.eval(s"$fnName()").toString
}
