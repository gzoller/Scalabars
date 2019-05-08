package co.blocke.scalabars

case class EachHelper() extends Helper {
  def eval(context: Context, expr: Expression)(implicit sb: Scalabars): String = "each"
}
