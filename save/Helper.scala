package co.blocke.scalabars

import org.json4s._

trait Helper {
  val args: List[String]

  def eval(context: Context, expr: Expression)(implicit sb: Scalabars): String = {
    val (assignments, literalsAndPaths) = expr.args.partition(_.isInstanceOf[AssignmentArgument])
    literalsAndPaths.map( _ match {
      case s:StringArgument => Context(JString(s.value))
      case p:PathArgument => context.find(p.path, options)
    })
//    val options = Options(expr.label, expr.)
  }

  def resolve(context: Context, expr: Expression, path: String): String = {
    // Helper arg path is a.b.c
    // First part is either "this", "@root", or an argument
    val parts = path.split(".")
    val argPos = args.indexOf(parts.head)
    if(argPos >= 0) {
      expr.args match {
        case s: StringArgument => s
        case p: PathArgument =>
        case a: AssignmentArgument
      }
    }
  }
}

case class Options(
                    helperName: String,
                    blockParms: List[String],
                    params: List[Context],
                    context: Context,  // i.e. the stack, the global context, etc.
                    fn: ()=>String,
                    handlebars: Scalabars
                  )
