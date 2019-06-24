package co.blocke.scalabars
package model

import renderables._

trait HelperTagCommon {
  // Do everything necessary to update Options before eval() on this helper.  Set up any current state
  // held in Options or to be passed down by parent.
  def bakeOptions(name: String, expr: ParsedExpression, options: Options): Options = {
    val (assignments, literalsAndPaths) = expr.args.partition(_.isInstanceOf[AssignmentArgument]).asInstanceOf[(Seq[AssignmentArgument], Seq[Argument])]

    // Poke assignments into Options.hash
    val hashAdds: Seq[(String, EvalResult[_])] = assignments.map(a => (a.label, a.targetValue.eval(options)))

    // Now eval all literals and paths and put EvalResults into params
    val evaledParams = literalsAndPaths.map(_.eval(options)).toList

    // Cook the newly populated Options
    options.copy(
      helperName  = name,
      params      = evaledParams,
      paramValues = literalsAndPaths.map(_.value).toList,
      _hash       = options._hash ++ hashAdds.toMap)
  }

  // Eval comes back with a String.  This converts the String into:  Whitespace, Text, Whitespace
  def sliceToRenderables(s: String): Seq[Renderable] = {
    val (wsBefore, rest) = s.indexWhere(c => !c.isWhitespace) match {
      case -1 => (Whitespace(""), s)
      case i  => (Whitespace(s.take(i)), s.drop(i))
    }
    val (body, wsAfter) = rest.lastIndexWhere(c => !c.isWhitespace) match {
      case -1 => (rest, Whitespace(""))
      case i  => (rest.take(i + 1), Whitespace(rest.drop(i + 1)))
    }
    Seq(wsBefore, Text(body), wsAfter)
  }
}

