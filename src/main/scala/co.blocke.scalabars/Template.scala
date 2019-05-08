package co.blocke.scalabars

import scala.reflect.runtime.universe.TypeTag

case class Template(compiled: List[Renderable], options: Options) {

  def render[T](contextObj: T)(implicit tt: TypeTag[T]): String = render(Context(options.handlebars.toJson4s(contextObj)))

  def render(context: Context): String = compiled.map(_.render(options.copy(context = Some(context)))).mkString("")
}
