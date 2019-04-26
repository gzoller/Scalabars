package co.blocke.scalabars

import reflect.runtime.universe.TypeTag

case class ScalaBars(template: String, partials: Map[String, String] = Map.empty[String, String]) {

  private val parser = HandlebarsParser()
  private val parsed = parser.compile(template)
  private var compiledPartials = Map.empty[String, List[Renderable]]

  def registerPartial(name: String, template: String) = this.copy(partials = this.partials + (name -> template))

  def render[T](context: T)(implicit tt: TypeTag[T]): String = render(SB.sj.render(context))
  def render(context: SB.Scope) = {
    if (compiledPartials.isEmpty && partials.nonEmpty)
      compiledPartials = partials.map { case (k, v) => (k, parser.compile(v)) }
    parsed.map(_.render(Context(context))(this)).mkString("")
  }

  private[scalabars] def getPartial(name: String): Option[List[Renderable]] = compiledPartials.get(name)
}
