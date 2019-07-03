package co.blocke.scalabars
package model

import renderables._

trait HelperTagCommon {
  // Do everything necessary to update Options before eval() on this helper.  Set up any current state
  // held in Options or to be passed down by parent.
  def bakeOptions(name: String, expr: ParsedExpression, options: Options): Options = {
    val (assignments, literalsAndPaths) =
      expr.args.partition(_.isInstanceOf[AssignmentArgument]).asInstanceOf[(Seq[AssignmentArgument], Seq[Argument])]

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
}
