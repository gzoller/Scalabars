package co.blocke.scalabars

import scala.reflect.runtime.universe.TypeTag

trait Template {
  val compiled: List[Renderable]
  val options: Options
  def render[T](contextObj: T)(implicit tt: TypeTag[T]): String
  def render(context: Context): String
}

case class SimpleTemplate(compiled: List[Renderable], options: Options) extends Template {
  def render[T](contextObj: T)(implicit tt: TypeTag[T]): String = render(Context(options.handlebars.toJson4s(contextObj)))
  def render(context: Context): String = compiled.map(_.render(options.copy(context = Some(context)))).mkString("")
}

//object NoopTemplate extends Template {
//  val compiled = List.empty[Renderable]
//  val options: Options = Options(null)
//  def render[T](contextObj: T)(implicit tt: TypeTag[T]): String = ""
//  def render(context: Context): String = ""
//}
