package co.blocke.scalabars
package model

import org.json4s.JString

case class ExpressionHelper(e: ExpressionArgument, isPartial: Boolean) extends Helper() {

  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_] = {
    // Expression must eval to a string, since this value will select what helper (or partial) will be selected next.
    val name = e.eval(options) match {
      case s: StringEvalResult => s.value
      // $COVERAGE-OFF$This doesn't seem possible to trigger, but left in for safety.
      case ContextEvalResult(Context(_, s: JString, _, _, _, _)) => s.values
      // $COVERAGE-ON$
      case _ => throw new BarsException("Expression helpers must resolve to a String")
    }
    val helper =
      if (isPartial)
        options.handlebars
          .getPartial(name)
          .getOrElse(throw new BarsException(s"Expression eval to '${name}' but no partial by this name is registered."))
      else
        options.handlebars
          .getHelper(name)
          .getOrElse(throw new BarsException(s"Expression eval to '${name}' but no helper by this name is registered."))
    helper.run()(options.copy(helperName = name), partials)
  }
}
