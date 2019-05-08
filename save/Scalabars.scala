package co.blocke.scalabars

case class Scalabars(helpers: Map[String,Helper] = Map(
  "each" -> EachHelper()
)) {

  private lazy val parser = HandlebarsParser()

  def getHelper(name: String) = helpers.get(name)

  def compile(t: String, options: Map[String, Any] = Map.empty[String, Any]) = Template(parser.compile(t))(this, options)

  //  private val parser = HandlebarsParser()
  //  val parsed = parser.compile(template)
  //  private var compiledPartials = Map.empty[String, List[Renderable]]
  //  private var hasEvaledHelpers = false
  //  private val javascript = new ScriptEngineManager().getEngineByName("nashorn")

  //  def registerPartial(name: String, template: String): ScalaBars = this.copy(partials = this.partials + (name -> template))
  def registerHelper(name: String, helper: Helper): Scalabars = this.copy(helpers = this.helpers + (name -> helper))

  /*
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
   */
}
